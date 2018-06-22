#include "hiredis/hiredis.h"

#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <stdio.h>
#include <string.h>

value Some(value x) {
    value dst = caml_alloc_small(1, 0);
    Store_field(dst, 0, x);
    return dst;
}

const value None = Val_int(0);
#define Nil None
#define OK None

value ERR(char *s) {
    value dst = caml_alloc_small(1, 1);
    Store_field(dst, 0, s == NULL ? None : Some (caml_copy_string(s)));
    return dst;
}

value convert_reply(redisReply *reply, int consume, int *valid){
    value dst = Val_unit;

    if (!reply){
        *valid = 0;
        return dst;
    }

    *valid = 1;
    if (reply->type == REDIS_REPLY_ERROR){
        dst = caml_alloc_small(1, 0);
        value s = caml_alloc_string(reply->len);
        memcpy(String_val(s), reply->str, reply->len);
        Store_field(dst, 0, s);
    } else if (reply->type == REDIS_REPLY_STATUS){
        dst = caml_alloc_small(1, 4);
        value s = caml_alloc_string(reply->len);
        memcpy(String_val(s), reply->str, reply->len);
        Store_field(dst, 0, s);
    } else if (reply->type == REDIS_REPLY_STRING){
        if (reply-> len < 0){
            dst = Nil;
        } else {
            dst = caml_alloc_small(1, 2);
            value s = caml_alloc_string(reply->len);
            memcpy(String_val(s), reply->str, reply->len);
            Store_field(dst, 0, s);
        }
    } else if (reply->type == REDIS_REPLY_INTEGER){
        dst = caml_alloc_small(1, 1);
        Store_field(dst, 0, caml_copy_int64(reply->integer));
    } else if (reply->type == REDIS_REPLY_ARRAY){
        if (reply->elements < 0){
            dst = Nil;
        } else {
            dst = caml_alloc_small(1, 3);
            value s = caml_alloc(reply->elements, 0);
            for(size_t i = 0; i < reply->elements; i++){
                Store_field(s, i, convert_reply(reply->element[i], 0, valid));
                if (!valid) break;
            }
            Store_field(dst, 0, s);
        }
    } else {
        dst = Nil;
    }

    if (consume){
        freeReplyObject(reply);
    }

    return dst;
}

value redis_context_errstr (value _ctx){
    CAMLparam1(_ctx);
    redisContext *ctx = (redisContext*)_ctx;
    if (strlen(ctx->errstr) == 0){
        CAMLreturn(None);
    }
    CAMLreturn (Some (caml_copy_string(ctx->errstr)));
}

value redis_context_to_fd (value _ctx){
    CAMLparam1(_ctx);
    redisContext *ctx = (redisContext*)_ctx;
    CAMLreturn (Val_int(ctx->fd));
}

value redis_context_of_fd (value _fd){
    int fd = Int_val(_fd);

    if (fd < 0){
        caml_failwith("unable to create context");
        return Val_unit;
    }

    caml_release_runtime_system();
    redisContext *ctx = redisConnectFd(fd);
    caml_acquire_runtime_system();
    if (!ctx){
        caml_failwith("unable to create context");
        return Val_unit;
    }

    return (value)ctx;
}

value redis_context_get_reply(value _ctx){
    CAMLparam1(_ctx);
    redisReply *reply = NULL;
    redisContext *ctx = (redisContext*)_ctx;

    caml_release_runtime_system();
    int res = redisGetReplyFromReader(ctx, (void**)&reply);
    caml_acquire_runtime_system();
    if (res != REDIS_OK){
        if (reply){
            freeReplyObject(reply);
        }
        CAMLreturn(None);
    }

    int valid;
    value response = convert_reply(reply, 1, &valid);
    CAMLreturn(valid ? Some(response) : None);
}

value redis_context_connect(value host, value port, value nonblock){
    CAMLparam2(host, port);

    redisContext *context = NULL;
    if (Bool_val(nonblock)){
        context = redisConnectNonBlock(String_val(host), Int_val(port));
    } else {
        context = redisConnect(String_val(host), Int_val(port));
    }

    if (context == NULL){
        caml_failwith ("unable to create context");
        CAMLreturn(Val_unit);
    }

    CAMLreturn((value)context);
}

value redis_context_connect_unix(value path, value nonblock){
    CAMLparam1(path);

    redisContext *context = NULL;
    if (Bool_val(nonblock)){
        context = redisConnectUnixNonBlock(String_val(path));
    } else {
        context = redisConnectUnix(String_val(path));
    }

    if (context == NULL){
        caml_failwith ("unable to create context");
        CAMLreturn(Val_unit);
    }

    CAMLreturn((value)context);
}

value redis_context_reconnect(value _ctx){
    CAMLparam1(_ctx);
    redisContext *ctx = (redisContext*)_ctx;
    CAMLreturn(redisReconnect(ctx) == REDIS_OK ? OK : ERR(ctx->errstr));
}

value redis_context_set_timeout (value _ctx, value s, value us){
    CAMLparam1(_ctx);
    redisContext *ctx = (redisContext*)_ctx;
    struct timeval tv;
    tv.tv_sec = Int_val(s);
    tv.tv_usec = Int_val(us);

    if (redisSetTimeout(ctx, tv) != REDIS_OK){
        CAMLreturn(ERR(ctx->errstr));
    }

    CAMLreturn(OK);
}

value redis_context_enable_keepalive(value _ctx){
    CAMLparam1(_ctx);
    redisContext *ctx = (redisContext*)_ctx;

    if (redisEnableKeepAlive(ctx) != REDIS_OK){
        CAMLreturn(ERR(ctx->errstr));
    }

    CAMLreturn(OK);
}

value redis_context_command(value _ctx, value arr){
    CAMLparam2(_ctx, arr);

    size_t argc = Wosize_val(arr);
    const char *argv[argc];
    size_t lens[argc];

    for (int i = 0; i < argc; i++){
        argv[i] = String_val(Field(arr, i));
        lens[i] = caml_string_length(Field(arr, i));
    }

    redisContext *ctx = (redisContext*)_ctx;
    caml_release_runtime_system();
    redisReply *reply = redisCommandArgv(ctx, argc, argv, lens);
    caml_acquire_runtime_system();

    int valid;
    value response = convert_reply(reply, 1, &valid);
    CAMLreturn (valid ? response : Nil);
}

value redis_context_append_command(value _ctx, value arr){
    CAMLparam2(_ctx, arr);

    size_t argc = Wosize_val(arr);
    const char *argv[argc];
    size_t lens[argc];
    redisContext *ctx = (redisContext*)_ctx;

    for (int i = 0; i < argc; i++){
        argv[i] = String_val(Field(arr, i));
        lens[i] = caml_string_length(Field(arr, i));
    }

    if (redisAppendCommandArgv(ctx, argc, argv, lens) != REDIS_OK){
        CAMLreturn(ERR(ctx->errstr));
    }

    CAMLreturn(OK);
}

value redis_context_append_formatted(value _ctx, value s){
    CAMLparam2(_ctx, s);

    redisContext *ctx = (redisContext*)_ctx;
    if (redisAppendFormattedCommand(ctx, String_val(s), caml_string_length(s)) != REDIS_OK){
        CAMLreturn(ERR(ctx->errstr));
    }

    CAMLreturn(OK);
}

value redis_format_command(value arr){
    CAMLparam1(arr);

    size_t argc = Wosize_val(arr);
    const char *argv[argc];
    size_t lens[argc];

    for (int i = 0; i < argc; i++){
        argv[i] = String_val(Field(arr, i));
        lens[i] = caml_string_length(Field(arr, i));
    }

    char *dst = NULL;
    int len = redisFormatCommandArgv(&dst, argc, argv, lens);
    if (len < 0){
        redisFreeCommand(dst);
        CAMLreturn(None);
    }

    CAMLlocal1(s);

    s = caml_alloc_string(len);
    memcpy(String_val(s), dst, len);
    redisFreeCommand(dst);

    CAMLreturn(Some(s));
}

value redis_context_flush_buffer (value _ctx){
    CAMLparam1(_ctx);
    int done = 0;
    redisContext *ctx = (redisContext*)_ctx;

    do {
        if (redisBufferWrite(ctx, &done) != REDIS_OK){
            CAMLreturn(ERR(ctx->errstr));
        }
    } while (!done);

    CAMLreturn (OK);
}

value redis_context_read_buffer (value _ctx){
    CAMLparam1(_ctx);

    redisContext *ctx = (redisContext*)_ctx;
    if (redisBufferRead(ctx) != REDIS_OK){
        CAMLreturn(ERR(ctx->errstr));
    }

    CAMLreturn(OK);
}

value redis_context_free_keep_fd(value _ctx){
    CAMLparam1(_ctx);
    redisFreeKeepFd((redisContext*)_ctx);
    CAMLreturn(Val_unit);
}

value redis_context_free(value _ctx){
    CAMLparam1(_ctx);
    redisFree((redisContext*)_ctx);
    CAMLreturn(Val_unit);
}

value redis_reader_create(value unit){
    redisReader *reader = redisReaderCreate();
    if (!reader){
        caml_failwith("unable to create reader");
        return Val_unit;
    }

    return (value)reader;
}

value redis_reader_free(value _reader){
    CAMLparam1(_reader);
    redisReaderFree((redisReader*)_reader);
    CAMLreturn(Val_unit);
}

value redis_reader_feed(value _reader, value s){
    CAMLparam2 (_reader, s);
    redisReader *reader = (redisReader*)_reader;

    if (redisReaderFeed(reader, String_val(s), caml_string_length(s)) != REDIS_OK){
        CAMLreturn(ERR(reader->errstr));
    }

    CAMLreturn(OK);
}

value redis_reader_get_reply(value _reader){
    CAMLparam1(_reader);
    redisReply *reply = NULL;
    redisReader *reader = (redisReader*)_reader;

    caml_release_runtime_system();
    if (redisReaderGetReply(reader, (void**)&reply) != REDIS_OK){
        if (reply){
            freeReplyObject(reply);
        }
        caml_acquire_runtime_system();
        CAMLreturn(None);
    }
    caml_acquire_runtime_system();
    int valid;
    value response = convert_reply(reply, 1, &valid);
    CAMLreturn(valid ? Some(response) : None);
}


