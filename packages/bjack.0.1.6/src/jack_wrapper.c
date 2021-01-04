/*
 * Copyright 2003-2006 Chris Morgan <cmorgan@alum.wpi.edu>
 * Copyright 2007-2008 Romain Beauxis <toots@rastageeks.org>
 *
 * This file is part of ocaml-bjack. It uses code from the bio2jack project.
 *
 * ocaml-bjack is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-bjack is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-taglib; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a
 * publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file under
 * terms of your choice, without any of the additional requirements listed in
 * clause 6 of the GNU Library General Public License. By "a publicly
 * distributed version of the Library", we mean either the unmodified Library as
 * distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library
 * General Public License. This exception does not however invalidate any other
 * reasons why the executable file might be covered by the GNU Library General
 * Public License.
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "jack_wrapper.h"

/* enable/disable TRACING through the JACK_Callback() function */
/* this can sometimes be too much information */
#define TRACE_CALLBACK 0

/* set to 1 for verbose output */
#define VERBOSE_OUTPUT 0

/* set to 1 to enable debug messages */
#define DEBUG_OUTPUT 0

/* set to 1 to enable tracing */
#define TRACE_ENABLE 0

/* set to 1 to enable the function timers */
#define TIMER_ENABLE 0

/* set to 1 to enable tracing of getDriver() and releaseDriver() */
#define TRACE_getReleaseDevice 0

#define ENABLE_WARNINGS 0

#define OUTFILE stderr

#if TIMER_ENABLE
/* This seemingly construct makes timing arbitrary functions really easy
   all you have to do is place a 'TIMER("start\n")' at the beginning and
   a 'TIMER("stop\n")' at the end of any function and this does the rest
   (naturally you can place any printf-compliant text you like in the argument
   along with the associated values). */
static struct timeval timer_now;
#define TIMER(format, args...)                                                 \
  gettimeofday(&timer_now, 0);                                                 \
  fprintf(OUTFILE, "%ld.%06ld: %s::%s(%d) " format, timer_now.tv_sec,          \
          timer_now.tv_usec, __FILE__, __FUNCTION__, __LINE__, ##args)
#else
#define TIMER(...)
#endif

#if TRACE_ENABLE
#define TRACE(format, args...)                                                 \
  fprintf(OUTFILE, "%s::%s(%d) " format, __FILE__, __FUNCTION__, __LINE__,     \
          ##args);                                                             \
  fflush(OUTFILE);
#else
#define TRACE(...)
#endif

#if DEBUG_OUTPUT
#define DEBUG(format, args...)                                                 \
  fprintf(OUTFILE, "%s::%s(%d) " format, __FILE__, __FUNCTION__, __LINE__,     \
          ##args);                                                             \
  fflush(OUTFILE);
#else
#define DEBUG(...)
#endif

#if TRACE_CALLBACK
#define CALLBACK_TRACE(format, args...)                                        \
  fprintf(OUTFILE, "%s::%s(%d) " format, __FILE__, __FUNCTION__, __LINE__,     \
          ##args);                                                             \
  fflush(OUTFILE);
#else
#define CALLBACK_TRACE(...)
#endif

#if ENABLE_WARNINGS
#define WARN(format, args...)                                                  \
  fprintf(OUTFILE, "WARN: %s::%s(%d) " format, __FILE__, __FUNCTION__,         \
          __LINE__, ##args);                                                   \
  fflush(OUTFILE);
#else
#define WARN(...)
#endif

#define ERR(format, args...)                                                   \
  fprintf(OUTFILE, "ERR: %s::%s(%d) " format, __FILE__, __FUNCTION__,          \
          __LINE__, ##args);                                                   \
  fflush(OUTFILE);

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) < (b)) ? (b) : (a))

/*
  Which SRC converter function we should use when doing sample rate conversion.
  Default to the fastest of the 'good quality' set.
 */
static int preferred_src_converter = SRC_SINC_FASTEST;

/* enable/disable code that allows us to close a device without actually closing
 * the jack device */
/* this works around the issue where jack doesn't always close devices by the
 * time the close function call returns */
#define JACK_CLOSE_HACK 1

typedef jack_default_audio_sample_t sample_t;
typedef jack_nframes_t nframes_t;

#if JACK_CLOSE_HACK
static void JACK_CloseDevice(jack_driver_t *drv, bool close_client);
#else
static void JACK_CloseDevice(jack_driver_t *drv);
#endif

/* Prototypes */
static int JACK_OpenDevice(jack_driver_t *drv);
static void JACK_CleanupDriver(jack_driver_t *drv);

/* Return the difference between two timeval structures in terms of milliseconds
 */
long TimeValDifference(struct timeval *start, struct timeval *end) {
  double long ms; /* milliseconds value */

  ms = end->tv_sec - start->tv_sec; /* compute seconds difference */
  ms *= (double)1000;               /* convert to milliseconds */

  ms += (double)(end->tv_usec - start->tv_usec) /
        (double)1000; /* add on microseconds difference */

  return (long)ms;
}

/* get a device and lock the devices mutex */
/* */
/* also attempt to reconnect to jack since this function is called from */
/* most other ocaml-bjack functions it provides a good point to attempt
 * reconnection */
/* */
/* Ok, I know this looks complicated and it kind of is. The point is that when
   you're trying to trace mutexes it's more important to know *who* called us
   than just that we were called.  This uses from pre-processor trickery so that
   the fprintf is actually placed in the function making the getDriver call.
   Thus, the __FUNCTION__ and __LINE__ macros will actually reference our
   caller, rather than getDriver.  The reason the fprintf call is passes as a
   parameter is because this macro has to still return a jack_driver_t* and we
   want to log both before *and* after the getDriver call for easier detection
   of blocked calls.
 */
// Beurk (toots)
#if TRACE_getReleaseDevice
#define getDriver(x)                                                           \
  _getDriver(x, fprintf(OUTFILE, "%s::%s(%d) getting driver %s\n", __FILE__,   \
                        __FUNCTION__, __LINE__, x->client_name));              \
  TRACE("got driver %s\n", x->client_name);
jack_driver_t *_getDriver(jack_driver_t *drv, int ignored) {
  fflush(OUTFILE);
#else
jack_driver_t *getDriver(jack_driver_t *drv) {
#endif

  if (pthread_mutex_lock(&drv->mutex) != 0)
    ERR("lock returned an error\n");

  /* should we try to restart the jack server? */
  if (drv->jackd_died && drv->client == 0) {
    struct timeval now;
    gettimeofday(&now, 0);

    /* wait 250ms before trying again */
    if (TimeValDifference(&drv->last_reconnect_attempt, &now) >= 250) {
      JACK_OpenDevice(drv);
      drv->last_reconnect_attempt = now;
    }
  }

  return drv;
}

/* release a device's mutex */
/* */
/* This macro is similar to the one for getDriver above, only simpler since we
   only really need to know when the lock was release for the sake of debugging.
*/
#if TRACE_getReleaseDevice
#define releaseDriver(x)                                                       \
  TRACE("releasing driver %s\n", x->client_name);                              \
  _releaseDriver(x);
void _releaseDriver(jack_driver_t *drv)
#else
void releaseDriver(jack_driver_t *drv)
#endif
{
  if (pthread_mutex_unlock(&drv->mutex) != 0)
    ERR("lock returned an error\n");
}

/* Return a string corresponding to the input state */
char *DEBUGSTATE(enum status_enum state) {
  if (state == PLAYING)
    return "PLAYING";
  else if (state == PAUSED)
    return "PAUSED";
  else if (state == STOPPED)
    return "STOPPED";
  else if (state == CLOSED)
    return "CLOSED";
  else if (state == RESET)
    return "RESET";
  else
    return "unknown state";
}

#define SAMPLE_MAX_16BIT 32768.0f
#define SAMPLE_MAX_8BIT 255.0f

/* floating point volume routine */
/* volume should be a value between 0.0 and 1.0 */
static void float_volume_effect(sample_t *buf, unsigned long nsamples,
                                float volume, int skip) {
  if (volume < 0)
    volume = 0;
  if (volume > 1.0)
    volume = 1.0;

  while (nsamples--) {
    *buf = (*buf) * volume;
    buf += skip;
  }
}

/* place one channel into a multi-channel stream */
static inline void mux(sample_t *dst, sample_t *src, unsigned long nsamples,
                       unsigned long dst_skip) {
  /* ALERT: signed sign-extension portability !!! */
  while (nsamples--) {
    *dst = *src;
    dst += dst_skip;
    src++;
  }
}

/* pull one channel out of a multi-channel stream */
static void demux(sample_t *dst, sample_t *src, unsigned long nsamples,
                  unsigned long src_skip) {
  /* ALERT: signed sign-extension portability !!! */
  while (nsamples--) {
    *dst = *src;
    dst++;
    src += src_skip;
  }
}

/* convert from 16 bit to floating point */
static inline void sample_move_short_float(sample_t *dst, short *src,
                                           unsigned long nsamples) {
  /* ALERT: signed sign-extension portability !!! */
  unsigned long i;
  for (i = 0; i < nsamples; i++)
    dst[i] = (sample_t)(src[i]) / SAMPLE_MAX_16BIT;
}

/* convert from floating point to 16 bit */
static inline void sample_move_float_short(short *dst, sample_t *src,
                                           unsigned long nsamples) {
  /* ALERT: signed sign-extension portability !!! */
  unsigned long i;
  for (i = 0; i < nsamples; i++)
    dst[i] = (short)((src[i]) * SAMPLE_MAX_16BIT);
}

/* convert from 8 bit to floating point */
static inline void sample_move_char_float(sample_t *dst, unsigned char *src,
                                          unsigned long nsamples) {
  /* ALERT: signed sign-extension portability !!! */
  unsigned long i;
  for (i = 0; i < nsamples; i++)
    dst[i] = (sample_t)(src[i]) / SAMPLE_MAX_8BIT;
}

/* convert from floating point to 8 bit */
static inline void sample_move_float_char(unsigned char *dst, sample_t *src,
                                          unsigned long nsamples) {
  /* ALERT: signed sign-extension portability !!! */
  unsigned long i;
  for (i = 0; i < nsamples; i++)
    dst[i] = (unsigned char)((src[i]) * SAMPLE_MAX_8BIT);
}

/* fill dst buffer with nsamples worth of silence */
static void inline sample_silence_float(sample_t *dst, unsigned long nsamples) {
  /* ALERT: signed sign-extension portability !!! */
  while (nsamples--) {
    *dst = 0;
    dst++;
  }
}

static bool inline ensure_buffer_size(char **buffer, unsigned long *cur_size,
                                      unsigned long needed_size) {
  DEBUG("current size = %lu, needed size = %lu\n", *cur_size, needed_size);
  if (*cur_size >= needed_size)
    return TRUE;
  DEBUG("reallocing\n");
  char *tmp = realloc(*buffer, needed_size);
  if (tmp) {
    *cur_size = needed_size;
    *buffer = tmp;
    return TRUE;
  }
  DEBUG("reallocing failed\n");
  return FALSE;
}

/******************************************************************
 *    JACK_callback
 *
 * every time the jack server wants something from us it calls this
 * function, so we either deliver it some sound to play or deliver it nothing
 * to play
 */
static int JACK_callback(nframes_t nframes, void *arg) {
  jack_driver_t *drv = (jack_driver_t *)arg;

  unsigned int i;
  int src_error = 0;

  TIMER("start\n");
  gettimeofday(&drv->previousTime, 0); /* record the current time */

  CALLBACK_TRACE("nframes %ld, sizeof(sample_t) == %d\n", (long)nframes,
                 sizeof(sample_t));

  if (!drv->client)
    ERR("client is closed, this is weird...\n");

  sample_t *out_buffer[MAX_OUTPUT_PORTS];
  /* retrieve the buffers for the output ports */
  for (i = 0; i < drv->num_output_channels; i++)
    out_buffer[i] =
        (sample_t *)jack_port_get_buffer(drv->output_port[i], nframes);

  sample_t *in_buffer[MAX_INPUT_PORTS];
  /* retrieve the buffers for the input ports */
  for (i = 0; i < drv->num_input_channels; i++)
    in_buffer[i] =
        (sample_t *)jack_port_get_buffer(drv->input_port[i], nframes);

  /* handle playing state */
  if (drv->state == PLAYING) {
    /* handle playback data, if any */
    if (drv->num_output_channels > 0) {
      unsigned long jackFramesAvailable =
          nframes;                    /* frames we have left to write to jack */
      unsigned long numFramesToWrite; /* num frames we are writing */
      size_t inputBytesAvailable = jack_ringbuffer_read_space(drv->pPlayPtr);
      unsigned long inputFramesAvailable; /* frames we have available */

      inputFramesAvailable =
          inputBytesAvailable / drv->bytes_per_jack_output_frame;
      size_t jackBytesAvailable =
          jackFramesAvailable * drv->bytes_per_jack_output_frame;

      long read = 0;

      CALLBACK_TRACE(
          "playing... jackFramesAvailable = %ld inputFramesAvailable = %ld\n",
          jackFramesAvailable, inputFramesAvailable);

#if JACK_CLOSE_HACK
      if (drv->in_use == FALSE) {
        /* output silence if nothing is being outputted */
        for (i = 0; i < drv->num_output_channels; i++)
          sample_silence_float(out_buffer[i], nframes);

        return -1;
      }
#endif

      /* make sure our buffer is large enough for the data we are writing */
      /* ie. callback_buffer2_size < (bytes we already wrote + bytes we are
       * going to write in this loop) */
      if (!ensure_buffer_size(&drv->callback_buffer2,
                              &drv->callback_buffer2_size,
                              jackBytesAvailable)) {
        ERR("allocated %lu bytes, need %lu bytes\n", drv->callback_buffer2_size,
            jackBytesAvailable);
        return -1;
      }

      /* do sample rate conversion if needed & requested */
      if (drv->output_src && drv->output_sample_rate_ratio != 1.0) {
        long bytes_needed_write = nframes * drv->bytes_per_jack_output_frame;

        /* make a very good guess at how many raw bytes we'll need to satisfy
         * jack's request after conversion */
        long bytes_needed_read =
            min(inputBytesAvailable,
                (double)(bytes_needed_write +
                         drv->output_sample_rate_ratio *
                             drv->bytes_per_jack_output_frame) /
                    drv->output_sample_rate_ratio);
        DEBUG("guessing that we need %ld bytes in and %ld out for rate "
              "conversion ratio = %f\n",
              bytes_needed_read, bytes_needed_write,
              drv->output_sample_rate_ratio);

        if (!ensure_buffer_size(&drv->callback_buffer1,
                                &drv->callback_buffer1_size,
                                bytes_needed_read)) {
          ERR("could not realloc callback_buffer2!\n");
          return 1;
        }
        if (!ensure_buffer_size(&drv->callback_buffer2,
                                &drv->callback_buffer2_size,
                                bytes_needed_write)) {
          ERR("could not realloc callback_buffer2!\n");
          return 1;
        }

        if (jackFramesAvailable && inputBytesAvailable > 0) {
          /* read in the data, but don't move the read pointer until we know how
           * much SRC used */
          jack_ringbuffer_peek(drv->pPlayPtr, drv->callback_buffer1,
                               bytes_needed_read);

          SRC_DATA srcdata;
          srcdata.data_in = (sample_t *)drv->callback_buffer1;
          srcdata.input_frames =
              bytes_needed_read / drv->bytes_per_jack_output_frame;
          srcdata.src_ratio = drv->output_sample_rate_ratio;
          srcdata.data_out = (sample_t *)drv->callback_buffer2;
          srcdata.output_frames = nframes;
          srcdata.end_of_input = 0; // it's a stream, it never ends
          DEBUG("input_frames = %ld, output_frames = %ld\n",
                srcdata.input_frames, srcdata.output_frames);
          /* convert the sample rate */
          src_error = src_process(drv->output_src, &srcdata);
          DEBUG("used = %ld, generated = %ld, error = %d: %s.\n",
                srcdata.input_frames_used, srcdata.output_frames_gen, src_error,
                src_strerror(src_error));

          if (src_error == 0) {
            /* now we can move the read pointer */
            jack_ringbuffer_read_advance(drv->pPlayPtr,
                                         srcdata.input_frames_used *
                                             drv->bytes_per_jack_output_frame);
            /* add on what we wrote */
            read = srcdata.input_frames_used * drv->bytes_per_output_frame;
            jackFramesAvailable -=
                srcdata.output_frames_gen; /* take away what was used */
          }
        }
      } else /* no resampling needed or requested */
      {
        /* read as much data from the buffer as is available */
        if (jackFramesAvailable && inputBytesAvailable > 0) {
          /* write as many bytes as we have space remaining, or as much as we
           * have data to write */
          numFramesToWrite = min(jackFramesAvailable, inputFramesAvailable);
          jack_ringbuffer_read(drv->pPlayPtr, drv->callback_buffer2,
                               jackBytesAvailable);
          /* add on what we wrote */
          read = numFramesToWrite * drv->bytes_per_output_frame;
          jackFramesAvailable -=
              numFramesToWrite; /* take away what was written */
        }
      }

      drv->written_client_bytes += read;
      drv->played_client_bytes +=
          drv->clientBytesInJack; /* move forward by the previous bytes we wrote
                                     since those must have finished by now */
      drv->clientBytesInJack =
          read; /* record the input bytes we wrote to jack */

      /* see if we still have jackBytesLeft here, if we do that means that we
         ran out of wave data to play and had a buffer underrun, fill in
         the rest of the space with zero bytes so at least there is silence */
      if (jackFramesAvailable) {
        WARN("buffer underrun of %ld frames\n", jackFramesAvailable);
        for (i = 0; i < drv->num_output_channels; i++)
          sample_silence_float(out_buffer[i] + (nframes - jackFramesAvailable),
                               jackFramesAvailable);
      }

      /* if we aren't converting or we are converting and src_error == 0 then we
       * should */
      /* apply volume and demux */
      if (!(drv->output_src && drv->output_sample_rate_ratio != 1.0) ||
          (src_error == 0)) {
        /* apply volume */
        for (i = 0; i < drv->num_output_channels; i++) {
          if (drv->volumeEffectType == dbAttenuation) {
            /* assume the volume setting is dB of attenuation, a volume of 0 */
            /* is 0dB attenuation */
            float volume = powf(10.0, -((float)drv->volume[i]) / 20.0);
            float_volume_effect((sample_t *)drv->callback_buffer2 + i,
                                (nframes - jackFramesAvailable), volume,
                                drv->num_output_channels);
          } else {
            float_volume_effect((sample_t *)drv->callback_buffer2 + i,
                                (nframes - jackFramesAvailable),
                                ((float)drv->volume[i] / 100.0),
                                drv->num_output_channels);
          }
        }

        /* demux the stream: we skip over the number of samples we have output
         * channels as the channel data */
        /* is encoded like chan1,chan2,chan3,chan1,chan2,chan3... */
        for (i = 0; i < drv->num_output_channels; i++) {
          demux(out_buffer[i], (sample_t *)drv->callback_buffer2 + i,
                (nframes - jackFramesAvailable), drv->num_output_channels);
        }
      }
    }

    /* handle record data, if any */
    if (drv->num_input_channels > 0) {
      long jack_bytes =
          nframes * drv->bytes_per_jack_input_frame; /* how many bytes jack is
                                                        feeding us */

      if (!ensure_buffer_size(&drv->callback_buffer1,
                              &drv->callback_buffer1_size, jack_bytes)) {
        ERR("allocated %lu bytes, need %lu bytes\n", drv->callback_buffer1_size,
            jack_bytes);
        return -1;
      }

      /* mux the invividual channels into one stream */
      for (i = 0; i < drv->num_input_channels; i++) {
        mux((sample_t *)drv->callback_buffer1 + i, in_buffer[i], nframes,
            drv->num_input_channels);
      }

      /* do sample rate conversion if needed & requested */
      if (drv->input_src && drv->input_sample_rate_ratio != 1.0) {
        /* make a very good guess at how many raw bytes we'll need to read all
         * the data jack gave us */
        long bytes_needed_write =
            (double)(jack_bytes + drv->input_sample_rate_ratio *
                                      drv->bytes_per_jack_input_frame) *
            drv->input_sample_rate_ratio;
        DEBUG("guessing that we need %ld bytes in and %ld out for rate "
              "conversion ratio = %f\n",
              nframes * drv->bytes_per_jack_input_frame, bytes_needed_write,
              drv->input_sample_rate_ratio);

        if (!ensure_buffer_size(&drv->callback_buffer2,
                                &drv->callback_buffer2_size,
                                bytes_needed_write)) {
          ERR("could not realloc callback_buffer2!\n");
          return 1;
        }

        SRC_DATA srcdata;
        srcdata.data_in = (sample_t *)drv->callback_buffer1;
        srcdata.input_frames = nframes;
        srcdata.src_ratio = drv->input_sample_rate_ratio;
        srcdata.data_out = (sample_t *)drv->callback_buffer2;
        srcdata.output_frames =
            drv->callback_buffer2_size / drv->bytes_per_jack_input_frame;
        srcdata.end_of_input = 0; // it's a stream, it never ends
        DEBUG("input_frames = %ld, output_frames = %ld\n", srcdata.input_frames,
              srcdata.output_frames);
        /* convert the sample rate */
        src_error = src_process(drv->input_src, &srcdata);
        DEBUG("used = %ld, generated = %ld, error = %d: %s.\n",
              srcdata.input_frames_used, srcdata.output_frames_gen, src_error,
              src_strerror(src_error));

        if (src_error == 0) {
          long write_space = jack_ringbuffer_write_space(drv->pRecPtr);
          long bytes_used =
              srcdata.output_frames_gen * drv->bytes_per_jack_input_frame;
          /* if there isn't enough room, do nothing. */
          if (write_space < bytes_used) {
            /* hey, we warn about underruns, we might as well warn about
             * overruns as well */
            WARN("buffer overrun of %ld bytes\n", jack_bytes - write_space);
          } else {
            jack_ringbuffer_write(drv->pRecPtr, drv->callback_buffer2,
                                  bytes_used);
          }
        }
      } else /* no resampling needed */
      {
        long write_space = jack_ringbuffer_write_space(drv->pRecPtr);
        /* if there isn't enough room, do nothing. */
        if (write_space < jack_bytes) {
          WARN("buffer overrun of %ld bytes\n", jack_bytes - write_space);
        } else {
          jack_ringbuffer_write(drv->pRecPtr, drv->callback_buffer1,
                                jack_bytes);
        }
      }
    }
  } else if (drv->state == PAUSED || drv->state == STOPPED ||
             drv->state == CLOSED || drv->state == RESET) {
    CALLBACK_TRACE("%s, outputting silence\n", DEBUGSTATE(drv->state));

    /* output silence if nothing is being outputted */
    for (i = 0; i < drv->num_output_channels; i++)
      sample_silence_float(out_buffer[i], nframes);

    /* if we were told to reset then zero out some variables */
    /* and transition to STOPPED */
    if (drv->state == RESET) {
      drv->written_client_bytes = 0;
      drv->played_client_bytes =
          0; /* number of the clients bytes that jack has played */

      drv->client_bytes = 0; /* bytes that the client wrote to use */

      drv->clientBytesInJack = 0; /* number of input bytes in jack(not necessary
                                     the number of bytes written to jack) */

      drv->position_byte_offset = 0;

      if (drv->pPlayPtr)
        jack_ringbuffer_reset(drv->pPlayPtr);

      if (drv->pRecPtr)
        jack_ringbuffer_reset(drv->pRecPtr);

      drv->state = STOPPED; /* transition to STOPPED */
    }
  }

  CALLBACK_TRACE("done\n");
  TIMER("finish\n");

  return 0;
}

/******************************************************************
 *             JACK_bufsize
 *
 *             Called whenever the jack server changes the the max number
 *             of frames passed to JACK_callback
 */
static int JACK_bufsize(nframes_t nframes, void *arg) {
  jack_driver_t *drv = (jack_driver_t *)arg;
  TRACE("the maximum buffer size is now %lu frames\n", (long)nframes);

  drv->jack_buffer_size = nframes;

  return 0;
}

/******************************************************************
 *		JACK_srate
 */
int JACK_srate(nframes_t nframes, void *arg) {
  jack_driver_t *drv = (jack_driver_t *)arg;

  drv->jack_sample_rate = (long)nframes;

  /* make sure to recalculate the ratios needed for proper sample rate
   * conversion */
  drv->output_sample_rate_ratio =
      (double)drv->jack_sample_rate / (double)drv->client_sample_rate;
  if (drv->output_src)
    src_set_ratio(drv->output_src, drv->output_sample_rate_ratio);

  drv->input_sample_rate_ratio =
      (double)drv->client_sample_rate / (double)drv->jack_sample_rate;
  if (drv->input_src)
    src_set_ratio(drv->input_src, drv->input_sample_rate_ratio);

  TRACE("the sample rate is now %lu/sec\n", (long)nframes);
  return 0;
}

/******************************************************************
 *		JACK_shutdown
 *
 * if this is called then jack shut down... handle this appropriately */
void JACK_shutdown(void *arg) {
  jack_driver_t *drv = (jack_driver_t *)arg;
  int n = strlen(drv->client_name) + 1;
  char *client_name = malloc(n);
  if (client_name == NULL) {
    ERR("Couldn't allocate %d bytes\n", n);
    return;
  }
  strcpy(client_name, drv->client_name);
  n = strlen(drv->server_name) + 1;
  char *server_name = malloc(n);
  if (server_name == NULL) {
    ERR("Couldn't allocate %d bytes\n", n);
    return;
  }
  strcpy(server_name, drv->server_name);

  TRACE("\n");

  getDriver(drv);

  drv->client = 0; /* reset client */
  drv->jackd_died = TRUE;

  TRACE("jack shutdown, setting client to 0 and jackd_died to true, closing "
        "device\n");

#if JACK_CLOSE_HACK
  JACK_CloseDevice(drv, TRUE);
#else
  JACK_CloseDevice(drv);
#endif

  TRACE("trying to reconnect right now\n");
  drv->client_name = client_name;
  drv->server_name = server_name;
  /* lets see if we can't reestablish the connection */
  if (JACK_OpenDevice(drv) != ERR_SUCCESS) {
    ERR("unable to reconnect with jack\n");
    free(client_name);
    free(server_name);
  }

  releaseDriver(drv);
}

/******************************************************************
 *		JACK_Error
 *
 * Callback for jack errors
 */
static void JACK_Error(const char *desc) { ERR("%s\n", desc); }

/******************************************************************
 *		JACK_OpenDevice
 *
 *  RETURNS: ERR_SUCCESS upon success
 */
static int JACK_OpenDevice(jack_driver_t *drv) {
  unsigned int i;
  int failed = 0;
  int options = JackNoStartServer | JackUseExactName;

  TRACE("creating jack client and setting up callbacks\n");

#if JACK_CLOSE_HACK
  /* see if this device is already open */
  if (drv->client) {
    /* if this device is already in use then it is bad for us to be in here */
    if (drv->in_use)
      return ERR_OPENING_JACK;

    TRACE("using existing client\n");
    drv->in_use = TRUE;
    return ERR_SUCCESS;
  }
#endif

  /* set up an error handler */
  jack_set_error_function(JACK_Error);

  /* try to become a client of the JACK server */
  TRACE("client name '%s'\n", drv->client_name);
  TRACE("server name '%s'\n", drv->server_name);
  if (strcmp(drv->server_name, "") != 0)
    options = JackServerName | options;
  if ((drv->client = jack_client_open(drv->client_name, options, NULL,
                                      drv->server_name)) == 0) {
    /* try once more */
    TRACE("trying once more to jack_client_open");
    if ((drv->client = jack_client_open(drv->client_name, options, NULL,
                                        drv->server_name)) == 0) {
      ERR("jack server not running?\n");
      return ERR_OPENING_JACK;
    }
  }

  TRACE("setting up jack callbacks\n");

  /* JACK server to call `JACK_callback()' whenever
     there is work to be done. */
  jack_set_process_callback(drv->client, JACK_callback, drv);

  /* setup a buffer size callback */
  jack_set_buffer_size_callback(drv->client, JACK_bufsize, drv);

  /* tell the JACK server to call `srate()' whenever
     the sample rate of the system changes. */
  jack_set_sample_rate_callback(drv->client, JACK_srate, drv);

  /* tell the JACK server to call `jack_shutdown()' if
     it ever shuts down, either entirely, or if it
     just decides to stop calling us. */
  jack_on_shutdown(drv->client, JACK_shutdown, drv);

  /* display the current sample rate. once the client is activated
     (see below), you should rely on your own sample rate
     callback (see above) for this value. */
  drv->jack_sample_rate = jack_get_sample_rate(drv->client);
  drv->output_sample_rate_ratio =
      (double)drv->jack_sample_rate / (double)drv->client_sample_rate;
  drv->input_sample_rate_ratio =
      (double)drv->client_sample_rate / (double)drv->jack_sample_rate;
  TRACE("client sample rate: %lu, jack sample rate: %lu, output ratio = %f, "
        "input ratio = %f\n",
        drv->client_sample_rate, drv->jack_sample_rate,
        drv->output_sample_rate_ratio, drv->input_sample_rate_ratio);

  drv->jack_buffer_size = jack_get_buffer_size(drv->client);

  /* create the output ports */
  TRACE("creating output ports\n");
  for (i = 0; i < drv->num_output_channels; i++) {
    char portname[32];
    sprintf(portname, "out_%d", i);
    TRACE("port %d is named '%s'\n", i, portname);
    /* NOTE: Yes, this is supposed to be JackPortIsOutput since this is an
     * output */
    /* port FROM ocaml-bjack */
    drv->output_port[i] = jack_port_register(
        drv->client, portname, JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  }

  /* create the input ports */
  TRACE("creating input ports\n");
  for (i = 0; i < drv->num_input_channels; i++) {
    char portname[32];
    sprintf(portname, "in_%d", i);
    TRACE("port %d is named '%s'\n", i, portname);
    /* NOTE: Yes, this is supposed to be JackPortIsInput since this is an input
     */
    /* port TO ocaml-bjack */
    drv->input_port[i] = jack_port_register(
        drv->client, portname, JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
  }

#if JACK_CLOSE_HACK
  drv->in_use = TRUE;
#endif

  /* tell the JACK server that we are ready to roll */
  TRACE("calling jack_activate()\n");
  if (jack_activate(drv->client)) {
    ERR("cannot activate client\n");
    return ERR_OPENING_JACK;
  } /* if( drv->num_input_channels > 0 ) */

  /* if something failed we need to shut the client down and return 0 */
  if (failed) {
    TRACE("failed, closing and returning error\n");
#if JACK_CLOSE_HACK
    JACK_CloseDevice(drv, TRUE);
#else
    JACK_CloseDevice(drv);
#endif
    return ERR_OPENING_JACK;
  }

  TRACE("success\n");

  drv->jackd_died = FALSE; /* clear out this flag so we don't keep attempting to
                              restart things */
  drv->state = PLAYING; /* clients seem to behave much better with this on from
                           the start, especially when recording */

  return ERR_SUCCESS; /* return success */
}

/******************************************************************
 *		JACK_CloseDevice
 *
 *	Close the connection to the server cleanly.
 *  If close_client is TRUE we close the client for this device instead of
 *    just marking the device as in_use(JACK_CLOSE_HACK only)
 */
#if JACK_CLOSE_HACK
static void JACK_CloseDevice(jack_driver_t *drv, bool close_client)
#else
static void JACK_CloseDevice(jack_driver_t *drv)
#endif
{

#if JACK_CLOSE_HACK
  if (close_client) {
#endif

    TRACE("closing the jack client thread\n");
    if (drv->client) {
      TRACE("after jack_deactivate()\n");
      int errorCode = jack_client_close(drv->client);
      if (errorCode)
        ERR("jack_client_close() failed returning an error code of %d\n",
            errorCode);
    }

    /* reset client */
    drv->client = 0;

    JACK_CleanupDriver(drv);

    drv->state = RESET;

#if JACK_CLOSE_HACK
  } else {
    TRACE("setting in_use to FALSE\n");
    drv->in_use = FALSE;

    if (!drv->client) {
      TRACE("critical error, closing a device that has no client\n");
    }
  }
#endif
}

/**************************************/
/* External interface functions below */
/**************************************/

/* Clear out any buffered data, stop playing, zero out some variables */
void JACK_Reset(jack_driver_t *drv) {
  getDriver(drv);
  /* NOTE: we use the RESET state so we don't need to worry about clearing out
   */
  /* variables that the callback modifies while the callback is running */
  /* we set the state to RESET and the callback clears the variables out for us
   */
  drv->state = RESET; /* tell the callback that we are to reset, the callback
                         will transition this to STOPPED */
  releaseDriver(drv);
}

/*
 * open the audio device for writing to
 *
 * if client is non-zero and in_use is FALSE then just set in_use to TRUE
 *
 * return value is zero upon success, non-zero upon failure
 */
int JACK_Open(jack_driver_t *drv, unsigned int bits_per_channel,
              unsigned long *rate, char *client_name, char *server_name,
              unsigned int input_channels, unsigned int output_channels,
              unsigned long jack_port_flags, int ringbuffer_size) {
  int retval;
  int n;

  if (input_channels < 1 && output_channels < 1) {
    ERR("no input OR output channels, nothing to do\n");
    return ERR_OPENING_JACK;
  }

  switch (bits_per_channel) {
  case 8:
  case 16:
    break;
  default:
    ERR("invalid bits_per_channel\n");
    return ERR_OPENING_JACK;
  }

  if (drv->allocated == TRUE) {
    ERR("Device already opened\n");
    return ERR_OPENING_JACK;
  }

  getDriver(drv);

  TRACE("bits_per_channel=%d rate=%ld, input_channels=%d, output_channels=%d\n",
        bits_per_channel, *rate, input_channels, output_channels);

  if (output_channels > MAX_OUTPUT_PORTS) {
    ERR("output_channels == %d, MAX_OUTPUT_PORTS == %d\n", output_channels,
        MAX_OUTPUT_PORTS);
    releaseDriver(drv);
    return ERR_TOO_MANY_OUTPUT_CHANNELS;
  }

  if (input_channels > MAX_INPUT_PORTS) {
    ERR("input_channels == %d, MAX_INPUT_PORTS == %d\n", input_channels,
        MAX_INPUT_PORTS);
    releaseDriver(drv);
    return ERR_TOO_MANY_INPUT_CHANNELS;
  }

  drv->jack_output_port_flags =
      jack_port_flags | JackPortIsInput; /* port must be input(ie we can put
                                            data into it), so mask this in */
  drv->jack_input_port_flags =
      jack_port_flags | JackPortIsOutput; /* port must be output(ie we can get
                                             data from it), so mask this in */

  /* initialize some variables */
  drv->in_use = FALSE;

  drv->state = RESET;

  /* drv->jack_sample_rate is set by JACK_OpenDevice() */
  drv->client_sample_rate = *rate;
  drv->bits_per_channel = bits_per_channel;
  drv->num_input_channels = input_channels;
  drv->num_output_channels = output_channels;
  n = strlen(client_name) + 1;
  if (n > jack_client_name_size()) {
    ERR("client_name length (%d) is greater than maximal possible length: %d\n",
        n, jack_client_name_size());
    return ERR_OPENING_JACK;
  }
  drv->client_name = malloc(n);
  if (drv->client_name == NULL) {
    ERR("Couldn't allocate %d bytes\n", n);
    return ERR_OPENING_JACK;
  }
  strcpy(drv->client_name, client_name);
  n = strlen(server_name) + 1;
  drv->server_name = malloc(n);
  if (drv->server_name == NULL) {
    ERR("Couldn't allocate %d bytes\n", n);
    return ERR_OPENING_JACK;
  }
  strcpy(drv->server_name, server_name);
  drv->bytes_per_input_frame =
      (drv->bits_per_channel * drv->num_input_channels) / 8;
  drv->bytes_per_output_frame =
      (drv->bits_per_channel * drv->num_output_channels) / 8;
  drv->bytes_per_jack_output_frame =
      sizeof(sample_t) * drv->num_output_channels;
  drv->bytes_per_jack_input_frame = sizeof(sample_t) * drv->num_input_channels;

  if (drv->num_output_channels > 0) {
    drv->pPlayPtr = jack_ringbuffer_create(drv->num_output_channels *
                                           drv->bytes_per_jack_output_frame *
                                           ringbuffer_size);
  }

  if (drv->num_input_channels > 0) {
    drv->pRecPtr = jack_ringbuffer_create(drv->num_input_channels *
                                          drv->bytes_per_jack_input_frame *
                                          ringbuffer_size);
  }

  DEBUG("bytes_per_output_frame == %ld\n", drv->bytes_per_output_frame);
  DEBUG("bytes_per_input_frame  == %ld\n", drv->bytes_per_input_frame);
  DEBUG("bytes_per_jack_output_frame == %ld\n",
        drv->bytes_per_jack_output_frame);
  DEBUG("bytes_per_jack_input_frame == %ld\n", drv->bytes_per_jack_input_frame);

  /* go and open up the device */
  retval = JACK_OpenDevice(drv);
  if (retval != ERR_SUCCESS) {
    TRACE("error opening jack device\n");
    releaseDriver(drv);
    return retval;
  } else {
    TRACE("succeeded opening jack device\n");
  }

  int error;
  if (drv->num_output_channels > 0) {
    drv->output_src =
        src_new(preferred_src_converter, drv->num_output_channels, &error);
    if (error != 0) {
      src_delete(drv->output_src);
      drv->output_src = 0;
      ERR("Could not created SRC object for output stream %d: %s\n", error,
          src_strerror(error));
    }
  }
  if (drv->num_input_channels > 0) {
    drv->input_src =
        src_new(preferred_src_converter, drv->num_input_channels, &error);
    if (error != 0) {
      src_delete(drv->input_src);
      drv->input_src = 0;
      ERR("Could not created SRC object for input stream %d: %s\n", error,
          src_strerror(error));
    }
  }

  drv->allocated = TRUE; /* record that we opened this device */

  DEBUG("sizeof(sample_t) == %d\n", sizeof(sample_t));

  int periodSize = jack_get_buffer_size(drv->client);
  int periods = 0;
  jack_latency_range_t range;
  /* FIXME: maybe we should keep different latency values for input vs output?
   */
  if (drv->num_output_channels > 0) {
    jack_port_get_latency_range(drv->output_port[0], JackPlaybackLatency,
                                &range);
    periods = range.max / periodSize;
    drv->latencyMS = periodSize * periods * 1000 /
                     (drv->jack_sample_rate *
                      (drv->bits_per_channel / 8 * drv->num_output_channels));
  } else if (drv->num_input_channels > 0) {
    jack_port_get_latency_range(drv->input_port[0], JackCaptureLatency, &range);
    periods = range.max / periodSize;
    drv->latencyMS = periodSize * periods * 1000 /
                     (drv->jack_sample_rate *
                      (drv->bits_per_channel / 8 * drv->num_input_channels));
  }

  TRACE("drv->latencyMS == %ldms\n", drv->latencyMS);

  releaseDriver(drv);
  return ERR_SUCCESS; /* success */
}

/* Close the jack device */
// FIXME: add error handling in here at some point...
/* NOTE: return 0 for success, non-zero for failure */
int JACK_Close(jack_driver_t *drv) {

  getDriver(drv);
  TRACE("client %s)\n", drv->client_name);

#if JACK_CLOSE_HACK
  JACK_CloseDevice(drv, TRUE);
#else
  JACK_CloseDevice(drv);
#endif

  drv->state = RESET;

  /* free buffer memory */
  drv->callback_buffer1_size = 0;
  if (drv->callback_buffer1)
    free(drv->callback_buffer1);
  drv->callback_buffer1 = 0;

  drv->callback_buffer2_size = 0;
  if (drv->callback_buffer2)
    free(drv->callback_buffer2);
  drv->callback_buffer2 = 0;

  drv->rw_buffer1_size = 0;
  if (drv->rw_buffer1)
    free(drv->rw_buffer1);
  drv->rw_buffer1 = 0;

  if (drv->pPlayPtr)
    jack_ringbuffer_free(drv->pPlayPtr);
  drv->pPlayPtr = 0;

  if (drv->pRecPtr)
    jack_ringbuffer_free(drv->pRecPtr);
  drv->pRecPtr = 0;

  /* free the SRC objects */
  if (drv->output_src)
    src_delete(drv->output_src);
  drv->output_src = 0;

  if (drv->input_src)
    src_delete(drv->input_src);
  drv->input_src = 0;

  drv->allocated = FALSE; /* release this device */

  if (drv->client_name)
    free(drv->client_name);
  drv->client_name = NULL;

  if (drv->server_name)
    free(drv->server_name);
  drv->server_name = NULL;

  releaseDriver(drv);

  return 0;
}

/* If we haven't already taken in the max allowed data then create a wave header
 */
/* to package the audio data and attach the wave header to the end of the */
/* linked list of wave headers */
/* These wave headers will be peeled off as they are played by the callback
 * routine */
/* Return value is the number of bytes written */
/* NOTE: this function takes the length of data to be written bytes */
long JACK_Write(jack_driver_t *drv, unsigned char *data, unsigned long bytes) {
  getDriver(drv);
  if (drv->in_use != TRUE) {
    ERR("Device not connected to jack!\n");
    return -1;
  }

  long frames_free, frames;

  TIMER("start\n");

  TRACE("client %d, bytes == %ld\n", drv->client_name, bytes);

  /* check and see that we have enough space for this audio */
  frames_free = jack_ringbuffer_write_space(drv->pPlayPtr) /
                drv->bytes_per_jack_output_frame;
  frames = bytes / drv->bytes_per_output_frame;
  TRACE("frames free == %ld, bytes = %lu\n", frames_free, bytes);

  TRACE("state = '%s'\n", DEBUGSTATE(drv->state));
  /* if we are currently STOPPED we should start playing now...
     do this before the check for bytes == 0 since some clients like
     to write 0 bytes the first time out */
  if (drv->state == STOPPED) {
    TRACE("currently STOPPED, transitioning to PLAYING\n");
    drv->state = PLAYING;
  }

  /* handle the case where the user calls this routine with 0 bytes */
  if (bytes == 0 || frames_free < 1) {
    TRACE("no room left\n");
    TIMER("finish (nothing to do, buffer is full)\n");
    releaseDriver(drv);
    return 0; /* indicate that we couldn't write any bytes */
  }

  frames = min(frames, frames_free);
  long jack_bytes = frames * drv->bytes_per_jack_output_frame;
  if (!ensure_buffer_size(&drv->rw_buffer1, &drv->rw_buffer1_size,
                          jack_bytes)) {
    ERR("couldn't allocate enough space for the buffer\n");
    releaseDriver(drv);
    return 0;
  }
  /* adjust bytes to be how many client bytes we're actually writing */
  bytes = frames * drv->bytes_per_output_frame;

  /* convert from client samples to jack samples
     we have to tell it how many samples there are, which is frames * channels
   */
  switch (drv->bits_per_channel) {
  case 8:
    sample_move_char_float((sample_t *)drv->rw_buffer1, (unsigned char *)data,
                           frames * drv->num_output_channels);
    break;
  case 16:
    sample_move_short_float((sample_t *)drv->rw_buffer1, (short *)data,
                            frames * drv->num_output_channels);
    break;
  }

  DEBUG("ringbuffer read space = %d, write space = %d\n",
        jack_ringbuffer_read_space(drv->pPlayPtr),
        jack_ringbuffer_write_space(drv->pPlayPtr));

  jack_ringbuffer_write(drv->pPlayPtr, drv->rw_buffer1, jack_bytes);
  DEBUG("wrote %lu bytes, %lu jack_bytes\n", bytes, jack_bytes);

  DEBUG("ringbuffer read space = %d, write space = %d\n",
        jack_ringbuffer_read_space(drv->pPlayPtr),
        jack_ringbuffer_write_space(drv->pPlayPtr));

  drv->client_bytes += bytes; /* update client_bytes */

  TIMER("finish\n");

  DEBUG("returning bytes written of %ld\n", bytes);

  releaseDriver(drv);
  return bytes; /* return the number of bytes we wrote out */
}

long JACK_Read(jack_driver_t *drv, unsigned char *data, unsigned long bytes) {
  getDriver(drv);
  if (drv->in_use != TRUE) {
    ERR("Device not connected to jack!\n");
    return -1;
  }

  long frames_available, frames;

  TIMER("start\n");

  TRACE("client %s, bytes == %ld\n", drv->client_name, bytes);

  /* find out if there's any data to read */
  frames_available = jack_ringbuffer_read_space(drv->pRecPtr) /
                     drv->bytes_per_jack_input_frame;
  frames = bytes / drv->bytes_per_input_frame;
  DEBUG("frames available = %ld, bytes = %lu\n", frames_available, bytes);

  TRACE("state = '%s'\n", DEBUGSTATE(drv->state));
  /* if we are currently STOPPED we should start recording now... */
  if (drv->state == STOPPED) {
    TRACE("currently STOPPED, transitioning to PLAYING\n");
    drv->state = PLAYING;
  }

  /* handle the case where the user calls this routine with 0 bytes */
  if (bytes == 0 || frames_available < 1) {
    TRACE("no bytes in buffer\n");

    TIMER("finish (nothing to do)\n");
    releaseDriver(drv);
    return 0;
  }

  frames = min(frames, frames_available);
  long jack_bytes = frames * drv->bytes_per_jack_input_frame;
  if (!ensure_buffer_size(&drv->rw_buffer1, &drv->rw_buffer1_size,
                          jack_bytes)) {
    ERR("couldn't allocate enough space for the buffer\n");
    releaseDriver(drv);
    return 0;
  }

  DEBUG("ringbuffer read space = %d, write space = %d\n",
        jack_ringbuffer_read_space(drv->pRecPtr),
        jack_ringbuffer_write_space(drv->pRecPtr));

  jack_ringbuffer_read(drv->pRecPtr, drv->rw_buffer1,
                       frames * drv->bytes_per_jack_input_frame);

  DEBUG("ringbuffer read space = %d, write space = %d\n",
        jack_ringbuffer_read_space(drv->pRecPtr),
        jack_ringbuffer_write_space(drv->pRecPtr));

  int i;
  for (i = 0; i < drv->num_output_channels; i++) {
    /* apply volume to the floating value */
    if (drv->volumeEffectType == dbAttenuation) {
      /* assume the volume setting is dB of attenuation, a volume of 0 */
      /* is 0dB attenuation */
      float volume = powf(10.0, -((float)drv->volume[i]) / 20.0);
      float_volume_effect((sample_t *)drv->rw_buffer1 + i, frames, volume,
                          drv->num_output_channels);
    } else {
      float_volume_effect((sample_t *)drv->rw_buffer1 + i, frames,
                          ((float)drv->volume[i] / 100.0),
                          drv->num_output_channels);
    }
  }

  /* convert from jack samples to client samples
     we have to tell it how many samples there are, which is frames * channels
   */
  switch (drv->bits_per_channel) {
  case 8:
    sample_move_float_char((unsigned char *)data, (sample_t *)drv->rw_buffer1,
                           frames * drv->num_input_channels);
    break;
  case 16:
    sample_move_float_short((short *)data, (sample_t *)drv->rw_buffer1,
                            frames * drv->num_input_channels);
    break;
  }

  TIMER("finish\n");

  long read_bytes = frames * drv->bytes_per_input_frame;

  DEBUG("returning bytes read of %ld\n", bytes);

  releaseDriver(drv);
  return read_bytes;
}

/* return ERR_SUCCESS for success */
int JACK_SetVolumeForChannel(jack_driver_t *drv, unsigned int channel,
                             unsigned int volume) {
  getDriver(drv);
  /* TODO?: maybe we should have different volume levels for input & output */
  /* ensure that we have the channel we are setting volume for */
  if (channel > (drv->num_output_channels - 1)) {
    releaseDriver(drv);
    return 1;
  }

  if (volume > 100)
    volume = 100; /* check for values in excess of max */

  drv->volume[channel] = volume;
  releaseDriver(drv);
  return ERR_SUCCESS;
}

/* Set the volume */
/* return 0 for success */
/* NOTE: we check for invalid volume values */
int JACK_SetAllVolume(jack_driver_t *drv, unsigned int volume) {
  unsigned int i;

  TRACE("client %s, setting volume of %d\n", drv->client_name, volume);

  for (i = 0; i < drv->num_output_channels; i++) {
    if (JACK_SetVolumeForChannel(drv, i, volume) != ERR_SUCCESS) {
      return 1;
    }
  }

  return ERR_SUCCESS;
}

/* Return the current volume in the inputted pointers */
/* NOTE: we check for null pointers being passed in just in case */
void JACK_GetVolumeForChannel(jack_driver_t *drv, unsigned int channel,
                              unsigned int *volume) {
  /* ensure that we have the channel we are getting volume for */
  if (channel > (drv->num_output_channels - 1)) {
    ERR("asking for channel index %d but we only have %ld channels\n", channel,
        drv->num_output_channels);
    return;
  }

  if (volume)
    *volume = drv->volume[channel];

#if VERBOSE_OUTPUT
  if (volume) {
    TRACE("client %s, returning volume of %d for channel %d\n",
          drv->client_name, *volume, channel);
  } else {
    TRACE("volume is null, can't dereference it\n");
  }
#endif
}

/* linear means 0 volume is silence, 100 is full volume */
/* dbAttenuation means 0 volume is 0dB attenuation */
/* ocaml-bjack defaults to linear */
enum JACK_VOLUME_TYPE JACK_SetVolumeEffectType(jack_driver_t *drv,
                                               enum JACK_VOLUME_TYPE type) {
  enum JACK_VOLUME_TYPE retval;
  getDriver(drv);

  TRACE("setting type of '%s'\n",
        (type == dbAttenuation ? "dbAttenuation" : "linear"));

  retval = drv->volumeEffectType;
  drv->volumeEffectType = type;

  releaseDriver(drv);
  return retval;
}

/* Controls the state of the playback(playing, paused, ...) */
int JACK_SetState(jack_driver_t *drv, enum status_enum state) {
  getDriver(drv);

  switch (state) {
  case PAUSED:
    drv->state = PAUSED;
    break;
  case PLAYING:
    drv->state = PLAYING;
    break;
  case STOPPED:
    drv->state = STOPPED;
    break;
  default:
    TRACE("unknown state of %d\n", state);
  }

  TRACE("%s\n", DEBUGSTATE(drv->state));

  releaseDriver(drv);
  return 0;
}

/* Retrieve the current state of the device */
enum status_enum JACK_GetState(jack_driver_t *drv) {
  enum status_enum return_val;

  return_val = drv->state;

  TRACE("client %s, returning current state of %s\n", drv->client_name,
        DEBUGSTATE(return_val));
  return return_val;
}

/* Retrieve the number of bytes per second we are outputting */
unsigned long JACK_GetOutputBytesPerSecond(jack_driver_t *drv) {
  return drv->bytes_per_output_frame * drv->client_sample_rate;
}

/* Retrieve the number of input bytes(from jack) per second we are outputting
   to the user of ocaml-bjack */
unsigned long JACK_GetInputBytesPerSecond(jack_driver_t *drv) {
  return drv->bytes_per_input_frame * drv->client_sample_rate;
}

/* An approximation of how many bytes we have to send out to jack */
/* that is computed as if we were sending jack a continuous stream of */
/* bytes rather than chunks during discrete callbacks.  */
/* Return the number of bytes we have buffered thus far for output */
/* NOTE: convert from output bytes to input bytes in here */
unsigned long JACK_GetBytesStored(jack_driver_t *drv) {
  getDriver(drv);
  if (drv->pPlayPtr == 0 || drv->bytes_per_jack_output_frame == 0) {
    releaseDriver(drv);
    return 0;
  }

  /* leave at least one frame in the buffer at all times to prevent underruns */
  long return_val =
      jack_ringbuffer_read_space(drv->pPlayPtr) - drv->jack_buffer_size;
  if (return_val <= 0) {
    return_val = 0;
  } else {
    /* adjust from jack bytes to client bytes */
    return_val = return_val / drv->bytes_per_jack_output_frame *
                 drv->bytes_per_output_frame;
  }
  releaseDriver(drv);
  return return_val;
}

/* Return the number of bytes we can write to the device */
unsigned long JACK_GetBytesFreeSpace(jack_driver_t *drv) {
  getDriver(drv);

  if (drv->pPlayPtr == 0 || drv->bytes_per_jack_output_frame == 0) {
    releaseDriver(drv);
    return 0;
  }

  /* leave at least one frame in the buffer at all times to prevent underruns */
  long return_val =
      jack_ringbuffer_write_space(drv->pPlayPtr) - drv->jack_buffer_size;
  if (return_val <= 0) {
    return_val = 0;
  } else {
    /* adjust from jack bytes to client bytes */
    return_val = return_val / drv->bytes_per_jack_output_frame *
                 drv->bytes_per_output_frame;
  }

  releaseDriver(drv);

  if (return_val < 0)
    return_val = 0;

  return return_val;
}

/* bytes of space used in the input buffer */
unsigned long JACK_GetBytesUsedSpace(jack_driver_t *drv) {
  long return_val;

  if (drv->pRecPtr == 0 || drv->bytes_per_jack_input_frame == 0) {
    return_val = 0;
  } else {
    /* adjust from jack bytes to client bytes */
    return_val = jack_ringbuffer_read_space(drv->pRecPtr) /
                 drv->bytes_per_jack_input_frame * drv->bytes_per_input_frame;
  }

  if (return_val < 0)
    return_val = 0;

  return return_val;
}

/* Get the current position of the driver, either in bytes or */
/* in milliseconds */
/* NOTE: this is position relative to input bytes, output bytes may differ
   greatly due to input vs. output channel count */
long JACK_GetPosition(jack_driver_t *drv, enum pos_enum position, int type) {
  long return_val = 0;
  struct timeval now;
  long elapsedMS;
  double sec2msFactor = 1000;

#if TRACE_ENABLE
  char *type_str = "UNKNOWN type";
#endif

  /* if we are reset we should return a position of 0 */
  if (drv->state == RESET) {
    TRACE("we are currently RESET, returning 0\n");
    return 0;
  }

  if (type == WRITTEN) {
#if TRACE_ENABLE
    type_str = "WRITTEN";
#endif
    return_val = drv->client_bytes;
  } else if (type == WRITTEN_TO_JACK) {
#if TRACE_ENABLE
    type_str = "WRITTEN_TO_JACK";
#endif
    return_val = drv->written_client_bytes;
  } else if (type ==
             PLAYED) /* account for the elapsed time for the played_bytes */
  {
#if TRACE_ENABLE
    type_str = "PLAYED";
#endif
    return_val = drv->played_client_bytes;
    gettimeofday(&now, 0);

    elapsedMS = TimeValDifference(
        &drv->previousTime,
        &now); /* find the elapsed milliseconds since last JACK_Callback() */

    TRACE("elapsedMS since last callback is '%ld'\n", elapsedMS);

    /* account for the bytes played since the last JACK_Callback() */
    /* NOTE: [Xms * (Bytes/Sec)] * (1 sec/1,000ms) */
    /* NOTE: don't do any compensation if no data has been sent to jack since
     * the last callback */
    /* as this would result a bogus computed result */
    if (drv->clientBytesInJack != 0) {
      return_val +=
          (long)((double)elapsedMS *
                 ((double)JACK_GetOutputBytesPerSecond(drv) / sec2msFactor));
    } else {
      TRACE("clientBytesInJack == 0\n");
    }
  }

  /* add on the offset */
  return_val += drv->position_byte_offset;

  /* convert byte position to milliseconds value if necessary */
  if (position == MILLISECONDS) {
    if (JACK_GetOutputBytesPerSecond(drv) != 0) {
      return_val = (long)(((double)return_val /
                           (double)JACK_GetOutputBytesPerSecond(drv)) *
                          (double)sec2msFactor);
    } else {
      return_val = 0;
    }
  }

#if TRACE_ENABLE
  TRACE("drv->client %s, type(%s), return_val = %ld\n", drv->client_name,
        type_str, return_val);
#endif
  return return_val;
}

// Set position always applies to written bytes
// NOTE: we must apply this instantly because if we pass this as a message
//   to the callback we risk the user sending us audio data in the mean time
//   and there is no need to send this as a message, we don't modify any
//   internal variables
void JACK_SetPosition(jack_driver_t *drv, enum pos_enum position, long value) {
  getDriver(drv);
  double sec2msFactor = 1000;
#if TRACE_ENABLE
  long input_value = value;
#endif

  /* convert the incoming value from milliseconds into bytes */
  if (position == MILLISECONDS) {
    value = (long)(((double)value * (double)JACK_GetOutputBytesPerSecond(drv)) /
                   sec2msFactor);
  }

  /* ensure that if the user asks for the position */
  /* they will at this instant get the correct position */
  drv->position_byte_offset = value - drv->client_bytes;

  TRACE("client %s input_value of %ld %s, new value of %ld, setting "
        "position_byte_offset to %ld\n",
        drv->client_name, input_value,
        (position == MILLISECONDS) ? "ms" : "bytes", value,
        drv->position_byte_offset);
  releaseDriver(drv);
}

/* Return the number of bytes per frame, or (output_channels * bits_per_channel)
 * / 8 */
unsigned long JACK_GetBytesPerOutputFrame(jack_driver_t *drv) {
  long return_val = drv->bytes_per_output_frame;
  TRACE("client %s, return_val = %ld\n", drv->client_name, return_val);
  return return_val;
}

/* Return the number of bytes per frame, or (input_channels * bits_per_channel)
 * / 8 */
unsigned long JACK_GetBytesPerInputFrame(jack_driver_t *drv) {
  return drv->bytes_per_input_frame;
}

/* Return the number of output bytes we buffer max */
long JACK_GetMaxOutputBufferedBytes(jack_driver_t *drv) {
  long return_val;

  if (drv->pPlayPtr == 0 || drv->bytes_per_jack_output_frame == 0)
    return_val = 0;

  /* adjust from jack bytes to client bytes */
  return_val = (jack_ringbuffer_read_space(drv->pPlayPtr) +
                jack_ringbuffer_write_space(drv->pPlayPtr)) /
               drv->bytes_per_jack_output_frame * drv->bytes_per_output_frame;

  TRACE("return_val = %ld\n", return_val);

  return return_val;
}

/* Return the number of input bytes we buffer max */
long JACK_GetMaxInputBufferedBytes(jack_driver_t *drv) {
  long return_val;

  if (drv->pRecPtr == 0 || drv->bytes_per_jack_input_frame == 0)
    return_val = 0;

  /* adjust from jack bytes to client bytes */
  return_val = (jack_ringbuffer_read_space(drv->pRecPtr) +
                jack_ringbuffer_write_space(drv->pRecPtr)) /
               drv->bytes_per_jack_input_frame * drv->bytes_per_input_frame;

  TRACE("return_val = %ld\n", return_val);

  return return_val;
}

/* Get the number of output channels */
int JACK_GetNumOutputChannels(jack_driver_t *drv) {
  int return_val = drv->num_output_channels;
  TRACE("getting num_output_channels of %d\n", return_val);
  return return_val;
}

/* Get the number of input channels */
int JACK_GetNumInputChannels(jack_driver_t *drv) {
  int return_val = drv->num_input_channels;
  TRACE("getting num_input_channels of %d\n", return_val);
  return return_val;
}

/* Get the number of samples per second, the sample rate */
long JACK_GetSampleRate(jack_driver_t *drv) {
  int return_val = drv->client_sample_rate;
  TRACE("getting sample_rate of %d\n", return_val);
  return return_val;
}

void JACK_CleanupDriver(jack_driver_t *drv) {
  TRACE("\n");
  /* things that need to be reset both in JACK_Init & JACK_CloseDevice */
  drv->client = 0;
  drv->in_use = FALSE;
  drv->state = CLOSED;
  drv->jack_sample_rate = 0;
  drv->output_sample_rate_ratio = 1.0;
  drv->input_sample_rate_ratio = 1.0;
  drv->jackd_died = FALSE;
  gettimeofday(&drv->previousTime, 0); /* record the current time */
  gettimeofday(&drv->last_reconnect_attempt, 0);
}

/* Initialize the jack porting library to a clean state */
jack_driver_t *JACK_CreateDriver(void) {
  jack_driver_t *drv = malloc(sizeof(jack_driver_t));
  if (drv == NULL)
    return NULL;
  memset(drv, 0, sizeof(jack_driver_t));
  int y;

  pthread_mutex_init(&drv->mutex, NULL);

  drv->volumeEffectType = linear;

  for (y = 0; y < MAX_OUTPUT_PORTS; y++) /* make all volume 100% as a default */
    drv->volume[y] = 100;

  JACK_CleanupDriver(drv);
  drv->state = RESET;

  drv->client_name = NULL;
  drv->server_name = NULL;

  return drv;
}

/* Get the latency, in frames, of jack */
long JACK_GetJackOutputLatency(jack_driver_t *drv) {
  long return_val = 0;
  jack_latency_range_t range;

  if (drv->client && drv->num_output_channels) {
    jack_port_get_latency_range(drv->output_port[0], JackPlaybackLatency,
                                &range);
    return_val = range.max;
  }

  TRACE("got latency of %ld frames\n", return_val);

  return return_val;
}

/* Get the latency, in frames, of jack */
long JACK_GetJackInputLatency(jack_driver_t *drv) {
  long return_val = 0;
  jack_latency_range_t range;

  if (drv->client && drv->num_input_channels) {
    jack_port_get_latency_range(drv->input_port[0], JackCaptureLatency, &range);
    return_val = range.max;
  }

  TRACE("got latency of %ld frames\n", return_val);

  return return_val;
}

/* bytes that jack requests during each callback */
unsigned long JACK_GetJackBufferedBytes(jack_driver_t *drv) {
  long return_val;

  if (drv->bytes_per_jack_output_frame == 0) {
    return_val = 0;
  } else {
    /* adjust from jack bytes to client bytes */
    return_val = drv->jack_buffer_size / drv->bytes_per_jack_output_frame *
                 drv->bytes_per_output_frame * drv->num_output_channels;
  }

  return return_val;
}

/* FIXME: put the filename of the resample library header file with the decoders
 * in here */
/* consider mapping them in the ocaml-bjack.h header file since its useless to
 * the user unless */
/* they can figure out wtf the settings on */
void JACK_SetSampleRateConversionFunction(int converter) {
  preferred_src_converter = converter;
}
