#define HTTPSERVER_IMPL
#include "httpserver.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>

typedef void (*request_handler)(struct http_request_s*);

CAMLprim value caml_http_response_init(value unit) {
    return (value)http_response_init();
}

CAMLprim value caml_http_request_has_flag(value v_req, value v_flag) {
    struct http_request_s* req = (struct http_request_s*)v_req;
    return Val_int(http_request_has_flag(req, Int_val(v_flag)));
}

CAMLprim value caml_http_response_status(value v_resp, value v_status) {
    struct http_response_s* resp = (struct http_response_s*)v_resp;
    http_response_status(resp, Int_val(v_status));
    return Val_unit;
}

CAMLprim value caml_http_response_header(value v_resp, value v_key,
                                         value v_value) {
    struct http_response_s* resp = (struct http_response_s*)v_resp;
    http_response_header(resp, String_val(v_key), String_val(v_value));
    return Val_unit;
}

CAMLprim value caml_http_response_body(value v_resp, value v_body) {
    struct http_response_s* resp = (struct http_response_s*)v_resp;
    http_response_body(resp, String_val(v_body), caml_string_length(v_body));
    return Val_unit;
}

CAMLprim value caml_http_respond(value v_req, value v_resp) {
    struct http_request_s* req = (struct http_request_s*)v_req;
    struct http_response_s* resp = (struct http_response_s*)v_resp;
    http_respond(req, resp);
    return Val_unit;
}

CAMLprim value caml_http_request_method(value v_req) {
    struct http_request_s* req = (struct http_request_s*)v_req;
    struct http_string_s ret = http_request_method(req);
    char tmp[100];
    strncpy(tmp, ret.buf, ret.len);
    tmp[ret.len] = '\0';
    return caml_copy_string(tmp);
}

CAMLprim value caml_http_request_body(value v_req) {
    struct http_request_s* req = (struct http_request_s*)v_req;
    struct http_string_s ret = http_request_body(req);
    char tmp[100];
    strncpy(tmp, ret.buf, ret.len);
    tmp[ret.len] = '\0';
    return caml_copy_string(tmp);
}

CAMLprim value caml_http_request_target(value v_req) {
    struct http_request_s* req = (struct http_request_s*)v_req;
    struct http_string_s ret = http_request_target(req);
    char tmp[100];
    strncpy(tmp, ret.buf, ret.len);
    tmp[ret.len] = '\0';
    return caml_copy_string(tmp);
}

CAMLprim value caml_http_request_header(value v_req, value v_key) {
    struct http_request_s* req = (struct http_request_s*)v_req;
    struct http_string_s ret = http_request_header(req, String_val(v_key));
    char tmp[100];
    strncpy(tmp, ret.buf, ret.len);
    tmp[ret.len] = '\0';
    return caml_copy_string(tmp);
}

void handle_request(struct http_request_s* request) {
    struct http_response_s* response = http_response_init();
    value unit = caml_callback2(*caml_named_value("caml_handle_request"),
                                (value)request, (value)response);
    http_respond(request, response);
}

void caml_http_server_init(value v_port) {
    struct http_server_s* server =
        http_server_init(Int_val(v_port), handle_request);
    http_server_listen(server);
}
