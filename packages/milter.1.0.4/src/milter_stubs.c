#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/in.h>
#include <sys/un.h> 

#include <libmilter/mfapi.h>

#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h> 
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/threads.h>

#define Some_val(v)    Field(v, 0)
#define Val_none       Val_int(0)

#define ENTER_CALLBACK                                                     \
    int __caml_milter_c_thread_registered = caml_c_thread_register();      \
    if (__caml_milter_c_thread_registered) caml_acquire_runtime_system();

#define LEAVE_CALLBACK                         \
    if (__caml_milter_c_thread_registered) {   \
        caml_release_runtime_system();         \
        caml_c_thread_unregister();            \
    }

static CAMLprim value
Val_some(value v)
{
    CAMLparam1(v);
    CAMLlocal1(r);

    r = caml_alloc(1, 0);
    Store_field(r, 0, v);

    CAMLreturn(r);
}

static void
milter_error(const char *err)
{
    caml_raise_with_string(*caml_named_value("Milter.Milter_error"), err);
}

CAMLprim value
caml_milter_opensocket(value rmsocket_val)
{
    CAMLparam1(rmsocket_val);
    int ret;
    int rmsocket = Bool_val(rmsocket_val);

    caml_release_runtime_system();
    ret = smfi_opensocket(rmsocket);
    caml_acquire_runtime_system();
    if (ret == MI_FAILURE)
        milter_error("Milter.opensocket");
    CAMLreturn(Val_unit);
}

static const int milter_flag_table[] = {
    SMFIF_ADDHDRS,
    SMFIF_CHGHDRS,
    SMFIF_CHGBODY,
    SMFIF_ADDRCPT,
    SMFIF_ADDRCPT_PAR,
    SMFIF_DELRCPT,
    SMFIF_QUARANTINE,
    SMFIF_CHGFROM,
    SMFIF_SETSYMLIST,
};

static const sfsistat milter_stat_table[] = {
    SMFIS_CONTINUE,
    SMFIS_REJECT,
    SMFIS_DISCARD,
    SMFIS_ACCEPT,
    SMFIS_TEMPFAIL,
    SMFIS_NOREPLY,
    SMFIS_SKIP,
    SMFIS_ALL_OPTS,
};

static unsigned long
milter_flags(value flag_list)
{
    CAMLparam1(flag_list);
    CAMLlocal1(head);
    unsigned long flags;

    flags = SMFIF_NONE;

    while (flag_list != Val_emptylist) {
        head = Field(flag_list, 0);
        flags |= milter_flag_table[Int_val(head)];
        flag_list = Field(flag_list, 1);
    }

    CAMLreturn(flags);
}

static value
alloc_ip(void *addr, size_t len)
{
    CAMLparam0();
    CAMLlocal1(res);

    res = caml_alloc_string(len);
    memcpy(String_val(res), addr, len);

    CAMLreturn(res);
}

static value
make_sockaddr(_SOCK_ADDR *sa)
{
    CAMLparam0();
    CAMLlocal1(res);

    switch (sa->sa_family) {
    case AF_UNIX: {
        struct sockaddr_un *sun = (struct sockaddr_un *)sa;
        res = caml_alloc(1, 0);
        Store_field(res, 0, caml_copy_string(sun->sun_path));
        break;
    }
    case AF_INET: {
        struct sockaddr_in *sin = (struct sockaddr_in *)sa;
        res = caml_alloc(2, 1);
        Store_field(res, 0, alloc_ip(&sin->sin_addr, 4));
        Store_field(res, 1, Val_int(ntohs(sin->sin_port)));
        break;
    }
    case AF_INET6: {
        struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)sa;
        res = caml_alloc(2, 1);
        Store_field(res, 0, alloc_ip(&sin6->sin6_addr, 16));
        Store_field(res, 1, Val_int(ntohs(sin6->sin6_port)));
        break;
    }
    default:
        milter_error("invalid family");
        break;
    }

    CAMLreturn(res);
}

static sfsistat
milter_connect(SMFICTX *ctx, char *host, _SOCK_ADDR *sockaddr)
{
    value ret, ctx_val, host_val, sockaddr_val;
    sfsistat s;
    static value *closure = NULL;

    ENTER_CALLBACK;

    ret = Val_none;
    ctx_val = (value)ctx;
    host_val = Val_none;
    sockaddr_val = Val_none;

    Begin_roots4(ret, ctx_val, host_val, sockaddr_val);

    if (host != NULL)
        host_val = Val_some(caml_copy_string(host));

    if (sockaddr != NULL)
        sockaddr_val = Val_some(make_sockaddr(sockaddr));

    if (closure == NULL)
        closure = caml_named_value("milter_connect");
    ret = caml_callback3(*closure, ctx_val, host_val, sockaddr_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_helo(SMFICTX *ctx, char *helo)
{
    value ret, ctx_val, helo_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ret = Val_none;
    ctx_val = (value)ctx;
    helo_val = caml_copy_string("");

    Begin_roots3(ret, ctx_val, helo_val);

    if (helo != NULL)
        helo_val = caml_copy_string(helo);

    if (closure == NULL)
        closure = caml_named_value("milter_helo");
    ret = caml_callback2(*closure, ctx_val, helo_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_envfrom(SMFICTX *ctx, char **envfrom)
{
    value ret, ctx_val, envfrom_val, args_val, args_tail;
    char **p;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = envfrom_val = args_val = args_tail = Val_none;

    Begin_roots5(ret, ctx_val, envfrom_val, args_val, args_tail);

    /* First element: envfrom */
    envfrom_val = caml_copy_string(*envfrom);

    /* Other elements: ESMTP arguments */
    args_tail = Val_emptylist;
    for (p = envfrom + 1; *p != NULL; p++) {
        args_val = caml_alloc(2, 0);
        Store_field(args_val, 0, caml_copy_string(*p));
        Store_field(args_val, 1, args_tail);
        args_tail = args_val;
    }

    if (closure == NULL)
        closure = caml_named_value("milter_envfrom");
    ret = caml_callback3(*closure, ctx_val, envfrom_val, args_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_envrcpt(SMFICTX *ctx, char **envrcpt)
{
    value ret, ctx_val, envrcpt_val, args_val, args_tail;
    char **p;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = envrcpt_val = args_val = args_tail = Val_none;

    Begin_roots5(ret, ctx_val, envrcpt_val, args_val, args_tail);

    /* First element: envrcpt */
    envrcpt_val = caml_copy_string(*envrcpt);

    /* Other elements: ESMTP arguments */
    args_tail = Val_emptylist;
    for (p = envrcpt + 1; *p != NULL; p++) {
        args_val = caml_alloc(2, 0);
        Store_field(args_val, 0, caml_copy_string(*p));
        Store_field(args_val, 1, args_tail);
        args_tail = args_val;
    }

    if (closure == NULL)
        closure = caml_named_value("milter_envrcpt");
    ret = caml_callback3(*closure, ctx_val, envrcpt_val, args_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_header(SMFICTX *ctx, char *headerf, char *headerv)
{
    value ret, ctx_val, headerf_val, headerv_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = headerf_val = headerv_val = Val_none;

    Begin_roots4(ret, ctx_val, headerf_val, headerv_val);

    headerf_val = caml_copy_string(headerf);
    headerv_val = caml_copy_string(headerv);

    if (closure == NULL)
        closure = caml_named_value("milter_header");
    ret = caml_callback3(*closure, ctx_val, headerf_val, headerv_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_eoh(SMFICTX *ctx)
{
    value ret, ctx_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = Val_none;

    Begin_roots2(ret, ctx_val);

    if (closure == NULL)
        closure = caml_named_value("milter_eoh");
    ret = caml_callback(*closure, ctx_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_body(SMFICTX *ctx, unsigned char *bodyp, size_t bodylen)
{
    value ret, ctx_val, body_val, len_val;
    static value *closure = NULL;
    intnat dims[] = { bodylen };
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = ctx_val = body_val = len_val = Val_unit;

    Begin_roots4(ret, ctx_val, body_val, len_val);

    body_val = caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, bodyp, dims);
    len_val = Val_int(bodylen);

    if (closure == NULL)
        closure = caml_named_value("milter_body");
    ret = caml_callback3(*closure, ctx_val, body_val, len_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_eom(SMFICTX *ctx)
{
    value ret, ctx_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = Val_none;

    Begin_roots2(ret, ctx_val);

    if (closure == NULL)
        closure = caml_named_value("milter_eom");
    ret = caml_callback(*closure, ctx_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_abort(SMFICTX *ctx)
{
    value ret, ctx_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = Val_none;

    Begin_roots2(ret, ctx_val);

    if (closure == NULL)
        closure = caml_named_value("milter_abort");
    ret = caml_callback(*closure, ctx_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_close(SMFICTX *ctx)
{
    value ret, ctx_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = Val_none;

    Begin_roots2(ret, ctx_val);

    if (closure == NULL)
        closure = caml_named_value("milter_close");
    ret = caml_callback(*closure, ctx_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_unknown(SMFICTX *ctx, const char *cmd)
{
    value ret, ctx_val, cmd_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = cmd_val = Val_none;

    Begin_roots3(ret, ctx_val, cmd_val);

    cmd_val = caml_copy_string(cmd);

    if (closure == NULL)
        closure = caml_named_value("milter_unknown");
    ret = caml_callback2(*closure, ctx_val, cmd_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static sfsistat
milter_data(SMFICTX *ctx)
{
    value ret, ctx_val;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = Val_none;

    Begin_roots2(ret, ctx_val);

    if (closure == NULL)
        closure = caml_named_value("milter_data");
    ret = caml_callback(*closure, ctx_val);

    s = milter_stat_table[Int_val(ret)];

    End_roots();

    LEAVE_CALLBACK;
    return s;
}

static const int milter_step_table[] = {
    SMFIP_NOCONNECT,
    SMFIP_NOHELO,
    SMFIP_NOMAIL,
    SMFIP_NORCPT,
    SMFIP_NOBODY,
    SMFIP_NOHDRS,
    SMFIP_NOEOH,
    SMFIP_NR_HDR,
    SMFIP_NOUNKNOWN,
    SMFIP_NODATA,
    SMFIP_SKIP,
    SMFIP_RCPT_REJ,
    SMFIP_NR_CONN,
    SMFIP_NR_HELO,
    SMFIP_NR_MAIL,
    SMFIP_NR_RCPT,
    SMFIP_NR_DATA,
    SMFIP_NR_UNKN,
    SMFIP_NR_EOH,
    SMFIP_NR_BODY,
    SMFIP_HDR_LEADSPC,
#ifdef SMFIP_MDS_256K
    SMFIP_MDS_256K,
#else
    0,
#endif
#ifdef SMFIP_MDS_1M
    SMFIP_MDS_1M,
#else
    0,
#endif
};

static sfsistat
milter_negotiate(SMFICTX *ctx,
                 unsigned long f0, unsigned long f1,
                 unsigned long f2, unsigned long f3,
                 unsigned long *pf0, unsigned long *pf1,
                 unsigned long *pf2, unsigned long *pf3)
{
    value ret, ctx_val, head;
    value actions_val, actions_tail;
    value steps_val, steps_tail;
    size_t i, len;
    static value *closure = NULL;
    sfsistat s;

    ENTER_CALLBACK;

    ctx_val = (value)ctx;
    ret = head = actions_val = actions_tail = steps_val = steps_tail = Val_none;

    Begin_roots3(ret, ctx_val, head);
    Begin_roots2(actions_val, actions_tail);
    Begin_roots2(steps_val, steps_tail);

    len = sizeof(milter_flag_table)/sizeof(milter_flag_table[0]);
    actions_tail = Val_emptylist;
    for (i = 0; i < len; i++) {
        if (f0 & milter_flag_table[i]) {
            actions_val = caml_alloc(2, 0);
            Store_field(actions_val, 0, Val_int(i));
            Store_field(actions_val, 1, actions_tail);
            actions_tail = actions_val;
        }
    }

    len = sizeof(milter_step_table)/sizeof(milter_step_table[0]);
    steps_tail = Val_emptylist;
    for (i = 0; i < len; i++) {
        if (f1 & milter_step_table[i]) {
            steps_val = caml_alloc(2, 0);
            Store_field(steps_val, 0, Val_int(i));
            Store_field(steps_val, 1, steps_tail);
            steps_tail = steps_val;
        }
    }

    if (closure == NULL)
        closure = caml_named_value("milter_negotiate");
    ret = caml_callback3(*closure, ctx_val, actions_val, steps_val);

    actions_val = Field(ret, 1);
    steps_val = Field(ret, 2);

    *pf0 = 0;
    while (actions_val != Val_emptylist) {
        head = Field(actions_val, 0);
        *pf0 |= milter_flag_table[Int_val(head)];
        actions_val = Field(actions_val, 1);
    }

    *pf1 = 0;
    while (steps_val != Val_emptylist) {
        head = Field(steps_val, 0);
        *pf1 |= milter_step_table[Int_val(head)];
        steps_val = Field(steps_val, 1);
    }

    *pf2 = 0;
    *pf3 = 0;

    s = milter_stat_table[Int_val(Field(ret, 0))];

    End_roots();
    End_roots();
    End_roots();

    LEAVE_CALLBACK;
    return s;
}

int
isnone(value opt)
{
    return (opt == Val_none);
}

CAMLprim value
caml_milter_register(value desc_val)
{
    CAMLparam1(desc_val);
    CAMLlocal1(ret);
    struct smfiDesc desc;

    desc.xxfi_name      = String_val(Field(desc_val, 0));
    desc.xxfi_version   = Int_val(Field(desc_val, 1));
    desc.xxfi_flags     = milter_flags(Field(desc_val, 2));
    desc.xxfi_connect   = isnone(Field(desc_val,  3)) ? NULL : milter_connect;
    desc.xxfi_helo      = isnone(Field(desc_val,  4)) ? NULL : milter_helo;
    desc.xxfi_envfrom   = isnone(Field(desc_val,  5)) ? NULL : milter_envfrom;
    desc.xxfi_envrcpt   = isnone(Field(desc_val,  6)) ? NULL : milter_envrcpt;
    desc.xxfi_header    = isnone(Field(desc_val,  7)) ? NULL : milter_header;
    desc.xxfi_eoh       = isnone(Field(desc_val,  8)) ? NULL : milter_eoh;
    desc.xxfi_body      = isnone(Field(desc_val,  9)) ? NULL : milter_body;
    desc.xxfi_eom       = isnone(Field(desc_val, 10)) ? NULL : milter_eom;
    desc.xxfi_abort     = isnone(Field(desc_val, 11)) ? NULL : milter_abort;
    desc.xxfi_close     = isnone(Field(desc_val, 12)) ? NULL : milter_close;
    desc.xxfi_unknown   = isnone(Field(desc_val, 13)) ? NULL : milter_unknown;
    desc.xxfi_data      = isnone(Field(desc_val, 14)) ? NULL : milter_data;
    desc.xxfi_negotiate = isnone(Field(desc_val, 15)) ? NULL : milter_negotiate;

    caml_release_runtime_system();
    ret = smfi_register(desc);
    caml_acquire_runtime_system();
    if (ret == MI_FAILURE)
        milter_error("Milter.register");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_setconn(value conn_value)
{
    CAMLparam1(conn_value);
    int ret;
    char *conn = strdup(String_val(conn_value));

    caml_release_runtime_system();
    ret = smfi_setconn(conn);
    caml_acquire_runtime_system();

    free(conn);

    if (ret == MI_FAILURE)
        milter_error("Milter.setconn");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_settimeout(value tmout_value)
{
    CAMLparam1(tmout_value);
    smfi_settimeout(Int_val(tmout_value));
    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_setbacklog(value backlog_value)
{
    CAMLparam1(backlog_value);
    int ret;

    ret = smfi_setbacklog(Int_val(backlog_value));
    if (ret == MI_FAILURE)
        milter_error("Milter.setbacklog");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_setdbg(value level_value)
{
    CAMLparam1(level_value);
    smfi_setdbg(Int_val(level_value));
    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_stop(value unit)
{
    CAMLparam1(unit);
    caml_release_runtime_system();
    smfi_stop();
    caml_acquire_runtime_system();
    CAMLreturn(Val_int(0)); /* SMFIS_CONTINUE */
}

CAMLprim value
caml_milter_main(value unit)
{
    CAMLparam1(unit);
    int ret;

    caml_release_runtime_system();
    ret = smfi_main();
    caml_acquire_runtime_system();
    if (ret == MI_FAILURE)
        milter_error("Milter.main");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_getsymval(value ctx_val, value sym_val)
{
    CAMLparam2(ctx_val, sym_val);
    CAMLlocal1(res);
    char *val;
    char *sym = String_val(sym_val);
    SMFICTX *ctx = (SMFICTX *)ctx_val;

    val = smfi_getsymval(ctx, sym);
    if (val == NULL)
        return Val_none;

    res = caml_alloc(1, 0);
    Store_field(res, 0, caml_copy_string(val));
    CAMLreturn(res);
}

struct milter_priv {
    value v;
};

CAMLprim value
caml_milter_getpriv(value ctx_val)
{
    CAMLparam1(ctx_val);
    CAMLlocal1(res);
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    struct milter_priv *p;

    p = smfi_getpriv(ctx);
    if (p == NULL)
        return Val_none;

    res = caml_alloc(1, 0);
    Store_field(res, 0, p->v);

    CAMLreturn(res);
}

CAMLprim value
caml_milter_setpriv(value ctx_val, value priv_opt)
{
    CAMLparam2(ctx_val, priv_opt);
    CAMLlocal1(priv);
    int ret, new_root;
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    struct milter_priv *p;

    p = smfi_getpriv(ctx);

    if (priv_opt == Val_none) {
        if (p != NULL) {
            caml_remove_generational_global_root(&(p->v));
            caml_stat_free(p);
            ret = smfi_setpriv(ctx, NULL);
            if (ret == MI_FAILURE)
                milter_error("Milter.setpriv");
        }
        CAMLreturn(Val_unit);
    }

    priv = Some_val(priv_opt);

    if (p == NULL) {
        p = caml_stat_alloc(sizeof(*p));
        p->v = priv;
        caml_register_generational_global_root(&(p->v));
        new_root = 1;
    } else {
        caml_modify_generational_global_root(&(p->v), priv);
        new_root = 0;
    }

    ret = smfi_setpriv(ctx, p);
    if (ret == MI_FAILURE) {
        if (new_root) {
            caml_stat_free(p);
            caml_remove_generational_global_root(&(p->v));
        }
        milter_error("Milter.setpriv");
    }

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_setreply(value ctx_val, value rcode_val, value xcode_val,
                     value msg_val)
{
    CAMLparam4(ctx_val, rcode_val, xcode_val, msg_val);
    int ret;
    char *msg;
    char *xcode;
    char *rcode = String_val(rcode_val);
    SMFICTX *ctx = (SMFICTX *)ctx_val;

    xcode = (xcode_val == Val_none)
          ? NULL
          : strdup(String_val(Some_val(xcode_val)));
    msg = (msg_val == Val_none) ? NULL : strdup(String_val(Some_val(msg_val)));

    caml_release_runtime_system();
    ret = smfi_setreply(ctx, rcode, xcode, msg);
    caml_acquire_runtime_system();

    if (xcode != NULL)
        free(xcode);
    if (msg != NULL)
        free(msg);

    if (ret == MI_FAILURE)
        milter_error("Milter.setreply");

    CAMLreturn(Val_unit);
}

#define SETMLREPLY_MAXLINES 32

CAMLprim value
caml_milter_setmlreply(value ctx_val, value rcode_val, value xcode_val,
                       value lines_val)
{
    CAMLparam4(ctx_val, rcode_val, xcode_val, lines_val);
    CAMLlocal1(line_val);
    int ret;
    size_t i, len;
    char *xcode;
    char **msg;
    char *rcode = strdup(String_val(rcode_val));
    SMFICTX *ctx = (SMFICTX *)ctx_val;

    msg = calloc(SETMLREPLY_MAXLINES, sizeof(char *));

    len = 0;
    while (lines_val != Val_emptylist && len <= SETMLREPLY_MAXLINES) {
        line_val = Field(lines_val, 0);
        msg[len] = strdup(String_val(line_val));
        lines_val = Field(lines_val, 1);
        len++;
    }
    if (lines_val != Val_emptylist) {
        /* List too long */
        ret = MI_FAILURE;
        goto cleanup;
    }

    xcode = (xcode_val == Val_none)
          ? NULL
          : strdup(String_val(Some_val(xcode_val)));

    caml_release_runtime_system();
    ret = smfi_setmlreply(ctx, rcode, xcode,
                          /* Fuck me */
                          msg[0],  msg[1],  msg[2],  msg[3],
                          msg[4],  msg[5],  msg[6],  msg[7],
                          msg[8],  msg[9],  msg[10], msg[11],
                          msg[12], msg[13], msg[14], msg[15],
                          msg[16], msg[17], msg[18], msg[19],
                          msg[20], msg[21], msg[22], msg[23],
                          msg[24], msg[25], msg[26], msg[27],
                          msg[28], msg[29], msg[30], msg[31]);
    caml_acquire_runtime_system();

    if (xcode != NULL)
        free(xcode);
cleanup:
    free(rcode);
    for (i = 0; i < len; i++)
        free(msg[i]);
    free(msg);

    if (ret == MI_FAILURE)
        milter_error("Milter.setmlreply");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_addheader(value ctx_val, value headerf_val, value headerv_val)
{
    CAMLparam3(ctx_val, headerf_val, headerv_val);
    int ret;
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *headerf = strdup(String_val(headerf_val));
    char *headerv = strdup(String_val(headerv_val));

    caml_release_runtime_system();
    ret = smfi_addheader(ctx, headerf, headerv);
    caml_acquire_runtime_system();

    free(headerf);
    free(headerv);

    if (ret == MI_FAILURE)
        milter_error("Milter.addheader");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_chgheader(value ctx_val,
                      value headerf_val, value idx_val, value headerv_val)
{
    CAMLparam4(ctx_val, headerf_val, idx_val, headerv_val);
    int ret;
    int32_t idx = Int_val(idx_val);
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *headerf = strdup(String_val(headerf_val));
    char *headerv = isnone(headerv_val)
                  ? NULL
                  : strdup(String_val(headerv_val));

    caml_release_runtime_system();
    ret = smfi_chgheader(ctx, headerf, idx, headerv);
    caml_acquire_runtime_system();

    free(headerf);
    if (headerv != NULL)
        free(headerv);

    if (ret == MI_FAILURE)
        milter_error("Milter.chgheader");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_insheader(value ctx_val, value idx_val,
                      value headerf_val, value headerv_val)
{
    CAMLparam4(ctx_val, headerf_val, idx_val, headerv_val);
    int ret;
    int idx = Int_val(idx_val);
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *headerf = strdup(String_val(headerf_val));
    char *headerv = strdup(String_val(headerv_val));

    caml_release_runtime_system();
    ret = smfi_insheader(ctx, idx, headerf, headerv);
    caml_acquire_runtime_system();

    free(headerf);
    free(headerv);

    if (ret == MI_FAILURE)
        milter_error("Milter.chgheader");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_chgfrom(value ctx_val, value mail_val, value args_val)
{
    CAMLparam3(ctx_val, mail_val, args_val);
    int ret;
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *mail = strdup(String_val(mail_val));
    char *args = args_val == Val_none
               ? NULL
               : strdup(String_val(Some_val(args_val)));

    caml_release_runtime_system();
    ret = smfi_chgfrom(ctx, mail, args);
    caml_acquire_runtime_system();

    free(mail);
    if (args != NULL)
        free(args);

    if (ret == MI_FAILURE)
        milter_error("Milter.chgfrom");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_addrcpt(value ctx_val, value rcpt_val)
{
    CAMLparam2(ctx_val, rcpt_val);
    int ret;
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *rcpt = strdup(String_val(rcpt_val));

    caml_release_runtime_system();
    ret = smfi_addrcpt(ctx, rcpt);
    caml_acquire_runtime_system();

    free(rcpt);

    if (ret == MI_FAILURE)
        milter_error("Milter.addrcpt");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_addrcpt_par(value ctx_val, value rcpt_val, value args_val)
{
    CAMLparam3(ctx_val, rcpt_val, args_val);
    int ret;
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *rcpt = strdup(String_val(rcpt_val));
    char *args = args_val == Val_none
               ? NULL
               : strdup(String_val(Some_val(args_val)));

    caml_release_runtime_system();
    ret = smfi_addrcpt_par(ctx, rcpt, args);
    caml_acquire_runtime_system();

    free(rcpt);
    if (args != NULL)
        free(args);

    if (ret == MI_FAILURE)
        milter_error("Milter.addrcpt_par");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_delrcpt(value ctx_val, value rcpt_val)
{
    CAMLparam2(ctx_val, rcpt_val);
    int ret;
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *rcpt = strdup(String_val(rcpt_val));

    caml_release_runtime_system();
    ret = smfi_delrcpt(ctx, rcpt);
    caml_acquire_runtime_system();

    free(rcpt);

    if (ret == MI_FAILURE)
        milter_error("Milter.delrcpt");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_replacebody(value ctx_val, value body_val)
{
    CAMLparam2(ctx_val, body_val);
    int ret;
    int len = caml_ba_byte_size(Caml_ba_array_val(body_val));
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    unsigned char *body = malloc(len);

    memcpy(body, Caml_ba_data_val(body_val), len);

    caml_release_runtime_system();
    ret = smfi_replacebody(ctx, body, len);
    caml_acquire_runtime_system();

    free(body);

    if (ret == MI_FAILURE)
        milter_error("Milter.replacebody");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_progress(value ctx_val)
{
    CAMLparam1(ctx_val);
    int ret;
    SMFICTX *ctx = (SMFICTX *)ctx_val;

    caml_release_runtime_system();
    ret = smfi_progress(ctx);
    caml_acquire_runtime_system();
    if (ret == MI_FAILURE)
        milter_error("Milter.progress");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_quarantine(value ctx_val, value reason_val)
{
    CAMLparam2(ctx_val, reason_val);
    int ret;
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *reason = strdup(String_val(reason_val));

    caml_release_runtime_system();
    ret = smfi_quarantine(ctx, reason);
    caml_acquire_runtime_system();

    free(reason);

    if (ret == MI_FAILURE)
        milter_error("Milter.quarantine");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_version(value unit)
{
    CAMLparam1(unit);
    CAMLlocal1(ret);
    unsigned int major, minor, pl;

    smfi_version(&major, &minor, &pl);

    ret = caml_alloc(3, 0);
    Store_field(ret, 0, Val_int(major));
    Store_field(ret, 1, Val_int(minor));
    Store_field(ret, 2, Val_int(pl));

    CAMLreturn(ret);
}

static const int milter_stage_table[] = {
    SMFIM_CONNECT,
    SMFIM_HELO,
    SMFIM_ENVFROM,
    SMFIM_ENVRCPT,
    SMFIM_DATA,
    SMFIM_EOM,
    SMFIM_EOH,
};

CAMLprim value
caml_milter_setsymlist(value ctx_val, value stage_val, value macros_val)
{
    CAMLparam3(ctx_val, stage_val, macros_val);
    int ret;
    int stage = milter_stage_table[Int_val(stage_val)];
    SMFICTX *ctx = (SMFICTX *)ctx_val;
    char *macros = strdup(String_val(macros_val));

    caml_release_runtime_system();
    ret = smfi_setsymlist(ctx, stage, macros);
    caml_acquire_runtime_system();

    free(macros);

    if (ret == MI_FAILURE)
        milter_error("Milter.setsymlist");

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_milter_version_code(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(SMFI_VERSION));
}
