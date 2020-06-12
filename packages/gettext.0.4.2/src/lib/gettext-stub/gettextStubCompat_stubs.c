/**************************************************************************/
/*  ocaml-gettext: a library to translate messages                        */
/*                                                                        */
/*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         */
/*                                                                        */
/*  This library is free software; you can redistribute it and/or         */
/*  modify it under the terms of the GNU Lesser General Public            */
/*  License as published by the Free Software Foundation; either          */
/*  version 2.1 of the License, or (at your option) any later version;    */
/*  with the OCaml static compilation exception.                          */
/*                                                                        */
/*  This library is distributed in the hope that it will be useful,       */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     */
/*  Lesser General Public License for more details.                       */
/*                                                                        */
/*  You should have received a copy of the GNU Lesser General Public      */
/*  License along with this library; if not, write to the Free Software   */
/*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   */
/*  USA                                                                   */
/**************************************************************************/

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <locale.h>
#include <libintl.h>

#define STRINGIFY(x) #x

/* Make a string option value from a const string which might be NULL. */
static value return_string_option (const char *str)
{
  CAMLparam0 ();
  CAMLlocal2 (rv, strv);

  if (str == NULL)
    rv = Val_int (0);		/* None */
  else {
    strv = caml_copy_string (str);
    rv = caml_alloc (1, 0);
    Store_field (rv, 0, strv);
  }

  CAMLreturn (rv);
}

int ml2c_lc_tab[7] = {
  LC_CTYPE,
  LC_NUMERIC,
  LC_TIME,
  LC_COLLATE,
  LC_MONETARY,
  LC_MESSAGES,
  LC_ALL
};

inline int ml2c_lc(value v)
{
  return ml2c_lc_tab[Int_val(v)];
}

CAMLprim value gettextStubCompat_setlocale(
	value v_n,
	value v_val)
{
  CAMLparam2(v_n, v_val);
  CAMLreturn(
      return_string_option (
        setlocale(
          ml2c_lc(v_n),
          String_val(v_val))));
}

CAMLprim value gettextStubCompat_gettext(
	value v_msgid)
{
  CAMLparam1(v_msgid);
  CAMLreturn(copy_string(gettext(String_val(v_msgid))));
}

CAMLprim value gettextStubCompat_dgettext(
	value v_domainname,
	value v_msgid)
{
  CAMLparam2(v_domainname, v_msgid);
  CAMLreturn(
      copy_string(
        dgettext(
          String_val(v_domainname),
          String_val(v_msgid))));
}

CAMLprim value gettextStubCompat_dcgettext(
	value v_domainname,
	value v_msgid,
	value v_category)
{
  CAMLparam3(v_domainname, v_msgid, v_category);
  CAMLreturn(
      copy_string(
        dcgettext(
          String_val(v_domainname),
          String_val(v_msgid),
          ml2c_lc(v_category))));
}

CAMLprim value gettextStubCompat_ngettext(
	value v_msgid1,
	value v_msgid2,
	value v_n)
{
  CAMLparam3(v_msgid1, v_msgid2, v_n);
  CAMLreturn(
      copy_string(
        ngettext(
          String_val(v_msgid1),
          String_val(v_msgid2),
          Long_val(v_n))));
}

CAMLprim value gettextStubCompat_dngettext(
	value v_domainname,
	value v_msgid1,
	value v_msgid2,
	value v_n)
{
  CAMLparam4(v_domainname, v_msgid1, v_msgid2, v_n);
  CAMLreturn(
      copy_string(
        dngettext(
          String_val(v_domainname),
          String_val(v_msgid1),
          String_val(v_msgid2),
          Long_val(v_n))));
}

CAMLprim value gettextStubCompat_dcngettext(
	value v_domainname,
	value v_msgid1,
	value v_msgid2,
	value v_n,
	value v_category)
{

  char *res = NULL;

  CAMLparam5(v_domainname, v_msgid1, v_msgid2, v_n, v_category);
  res = dcngettext(
      String_val(v_domainname),
      String_val(v_msgid1),
      String_val(v_msgid2),
      Long_val(v_n),
      ml2c_lc(v_category));

  if (res == NULL) {
    caml_failwith(
        "NULL string not expected at "STRINGIFY(__LINE__)" in "__FILE__);
  };

  CAMLreturn(copy_string(res));
}

CAMLprim value gettextStubCompat_textdomain(
	value v_domainname)
{
  CAMLparam1(v_domainname);
  CAMLreturn(return_string_option(textdomain(String_val(v_domainname))));
}

CAMLprim value gettextStubCompat_get_textdomain(value v_unit)
{
  CAMLparam1(v_unit);
  CAMLreturn(return_string_option(textdomain(NULL)));
}

CAMLprim value gettextStubCompat_bindtextdomain(
	value v_domainname,
	value v_dirname)
{
  CAMLparam2(v_domainname, v_dirname);
  CAMLreturn(
      return_string_option (
        bindtextdomain(
          String_val(v_domainname),
          String_val(v_dirname))));
}

CAMLprim value gettextStubCompat_bind_textdomain_codeset(
	value v_domainname,
	value v_codeset)
{
  CAMLparam2(v_domainname, v_codeset);
  CAMLreturn(
      return_string_option (
        bind_textdomain_codeset(
          String_val(v_domainname),
          String_val(v_codeset))));
}

