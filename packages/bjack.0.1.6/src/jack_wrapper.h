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

#ifndef _H_JACK_OUT_H
#define _H_JACK_OUT_H

#include <jack/jack.h>
#include <jack/ringbuffer.h>
#include <samplerate.h>
#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#else
#define bool long
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define ERR_SUCCESS 0
#define ERR_OPENING_JACK 1
#define ERR_BYTES_PER_OUTPUT_FRAME_INVALID 3
#define ERR_BYTES_PER_INPUT_FRAME_INVALID 4
#define ERR_TOO_MANY_OUTPUT_CHANNELS 5
#define ERR_PORT_NAME_OUTPUT_CHANNEL_MISMATCH 6
#define ERR_PORT_NOT_FOUND 7
#define ERR_TOO_MANY_INPUT_CHANNELS 8
#define ERR_PORT_NAME_INPUT_CHANNEL_MISMATCH 9

enum status_enum { PLAYING, PAUSED, STOPPED, CLOSED, RESET };
enum pos_enum { BYTES, MILLISECONDS };

#define MAX_OUTPUT_PORTS 10
#define MAX_INPUT_PORTS 10

/* linear means 0 volume is silence, 100 is full volume */
/* dbAttenuation means 0 volume is 0dB attenuation */
/* ocaml-bjack defaults to linear */
/* Note: volume controls only effect output channels for now */
enum JACK_VOLUME_TYPE { linear, dbAttenuation };

typedef struct jack_driver_s {
  bool allocated; /* whether or not this device has been allocated */

  long jack_sample_rate; /* jack samples(frames) per second */

  long client_sample_rate;         /* client samples(frames) per second */
  double output_sample_rate_ratio; /* ratio between jack's output rate & ours */
  double input_sample_rate_ratio;  /* ratio between our input rate & jack's */

  unsigned long num_input_channels;  /* number of input channels(1 is mono, 2
                                        stereo etc..) */
  unsigned long num_output_channels; /* number of output channels(1 is mono, 2
                                        stereo etc..) */

  unsigned long bits_per_channel; /* number of bits per channel (only 8 & 16 are
                                     currently supported) */

  unsigned long
      bytes_per_output_frame; /* (num_output_channels * bits_per_channel) / 8 */
  unsigned long
      bytes_per_input_frame; /* (num_input_channels * bits_per_channel) / 8 */

  unsigned long bytes_per_jack_output_frame; /* (num_output_channels *
                                                bits_per_channel) / 8 */
  unsigned long bytes_per_jack_input_frame;  /* (num_input_channels *
                                                bits_per_channel) / 8 */

  unsigned long latencyMS; /* latency in ms between writing and actual audio
                              output of the written data */

  long clientBytesInJack; /* number of INPUT bytes(from the client of
                             ocaml-bjack) we wrote to jack(not necessary the
                             number of bytes we wrote to jack) */
  long jack_buffer_size;  /* size of the buffer jack will pass in to the process
                             callback */

  unsigned long
      callback_buffer1_size; /* number of bytes in the buffer allocated for
                                processing data in JACK_Callback */
  char *callback_buffer1;
  unsigned long
      callback_buffer2_size; /* number of bytes in the buffer allocated for
                                processing data in JACK_Callback */
  char *callback_buffer2;

  unsigned long rw_buffer1_size; /* number of bytes in the buffer allocated for
                                    processing data in JACK_(Read|Write) */
  char *rw_buffer1;

  struct timeval previousTime; /* time of last JACK_Callback() write to jack,
                                  allows for MS accurate bytes played  */

  unsigned long
      written_client_bytes; /* input bytes we wrote to jack, not necessarily
                               actual bytes we wrote to jack due to channel and
                               other conversion */
  unsigned long played_client_bytes; /* input bytes that jack has played */

  unsigned long client_bytes; /* total bytes written by the client of
                                 ocaml-bjack via JACK_Write() */

  jack_port_t *output_port[MAX_OUTPUT_PORTS]; /* output ports */
  jack_port_t *input_port[MAX_OUTPUT_PORTS];  /* input ports */

  jack_client_t *client; /* pointer to jack client */
  char *client_name;

  /* Jack server where we should connect to. */
  char *server_name;

  unsigned long jack_output_port_flags; /* flags to be passed to jack when
                                           opening the output ports */
  unsigned long jack_input_port_flags;  /* flags to be passed to jack when
                                           opening the output ports */

  jack_ringbuffer_t *pPlayPtr; /* the playback ringbuffer */
  jack_ringbuffer_t *pRecPtr;  /* the recording ringbuffer */

  SRC_STATE *output_src; /* SRC object for the output stream */
  SRC_STATE *input_src;  /* SRC object for the output stream */

  enum status_enum
      state; /* one of PLAYING, PAUSED, STOPPED, CLOSED, RESET etc */

  unsigned int
      volume[MAX_OUTPUT_PORTS]; /* percentage of sample value to preserve, 100
                                   would be no attenuation */
  enum JACK_VOLUME_TYPE
      volumeEffectType; /* linear or dbAttenuation, if dbAttenuation volume is
                           the number of dBs of attenuation to apply, 0 volume
                           being no attenuation, full volume */

  long position_byte_offset; /* an offset that we will apply to returned
                                position queries to achieve */
  /* the position that the user of the driver desires set */

  bool in_use; /* true if this device is currently in use */

  pthread_mutex_t mutex; /* mutex to lock this specific device */

  /* variables used for trying to restart the connection to jack */
  bool jackd_died; /* true if jackd has died and we should try to restart it */
  struct timeval last_reconnect_attempt;
} jack_driver_t;

#define PLAYED                                                                 \
  1 /* played out of the speakers(estimated value but should be close */
#define WRITTEN_TO_JACK 2 /* amount written out to jack */
#define WRITTEN 3 /* amount written to the ocaml-bjack device */

/**********************/
/* External functions */
jack_driver_t *JACK_CreateDriver(void); /* This functions allocated memory. It
                                           should be freed by the programmer. */
void JACK_SetSampleRateConversionFunction(
    int converter); /* which SRC converter function should be used
                       for the next Open()d device */
int JACK_Open(jack_driver_t *drv, unsigned int bits_per_channel,
              unsigned long *rate, char *client_name, char *server_name,
              unsigned int input_channels, unsigned int output_channels,
              unsigned long jack_port_flags, int rb_size);
int JACK_Close(jack_driver_t *drv);  /* return 0 for success */
void JACK_Reset(jack_driver_t *drv); /* free all buffered data and reset several
                                        values in the device */
long JACK_Write(jack_driver_t *drv, unsigned char *data,
                unsigned long bytes); /* returns the number of bytes written, -1
                                         for fatal errors */
long JACK_Read(jack_driver_t *drv, unsigned char *data,
               unsigned long bytes); /* returns the number of bytes read, -1 for
                                        fatal errors */

/* state setting values */
/* set/get the written/played/buffered value based on a byte or millisecond
 * input value */
long JACK_GetPosition(jack_driver_t *drv, enum pos_enum position, int type);
void JACK_SetPosition(jack_driver_t *drv, enum pos_enum position, long value);

long JACK_GetJackLatency(jack_driver_t *drv); /* deprectated, you probably want
                                                 JACK_GetJackOutputLatency */
long JACK_GetJackOutputLatency(
    jack_driver_t *drv); /* return the output latency in frames */
long JACK_GetJackInputLatency(
    jack_driver_t *drv); /* return the input latency in frames */

int JACK_SetState(jack_driver_t *drv,
                  enum status_enum state); /* playing, paused, stopped */
enum status_enum JACK_GetState(jack_driver_t *drv);

long JACK_GetMaxOutputBufferedBytes(jack_driver_t *drv);
long JACK_GetMaxInputBufferedBytes(jack_driver_t *drv);

/* bytes that jack requests during each callback */
unsigned long JACK_GetJackBufferedBytes(jack_driver_t *drv);

/* Properties of the jack driver */

enum JACK_VOLUME_TYPE JACK_SetVolumeEffectType(jack_driver_t *drv,
                                               enum JACK_VOLUME_TYPE type);

int JACK_SetAllVolume(jack_driver_t *drv,
                      unsigned int volume); /* returns 0 on success */
int JACK_SetVolumeForChannel(jack_driver_t *drv, unsigned int channel,
                             unsigned int volume);
void JACK_GetVolumeForChannel(jack_driver_t *drv, unsigned int channel,
                              unsigned int *volume);

unsigned long JACK_GetOutputBytesPerSecond(
    jack_driver_t *drv); /* bytes_per_output_frame * sample_rate */
unsigned long JACK_GetInputBytesPerSecond(
    jack_driver_t *drv); /* bytes_per_input_frame * sample_rate */
unsigned long JACK_GetBytesStored(
    jack_driver_t *drv); /* bytes currently buffered in the output buffer */
unsigned long JACK_GetBytesFreeSpace(
    jack_driver_t *drv); /* bytes of free space in the output buffer */
unsigned long JACK_GetBytesUsedSpace(
    jack_driver_t *drv); /* bytes of space used in the input buffer */
unsigned long JACK_GetBytesPerOutputFrame(jack_driver_t *drv);
unsigned long JACK_GetBytesPerInputFrame(jack_driver_t *drv);

/* Note: these will probably be removed in a future release */
int JACK_GetNumInputChannels(jack_driver_t *drv);
int JACK_GetNumOutputChannels(jack_driver_t *drv);

long JACK_GetSampleRate(jack_driver_t *drv); /* samples per second */

#ifdef __cplusplus
}
#endif

#endif /* #ifndef JACK_OUT_H */
