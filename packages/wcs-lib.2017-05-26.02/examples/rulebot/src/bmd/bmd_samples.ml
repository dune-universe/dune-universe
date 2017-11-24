(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open Bmd_t
open Bmd_builder

(**

   an airplane is a business entity identified by an airplane id.
   an airplane has an average engine pressure ratio (integer).
   an airplane has an average engine rpm (integer).
   an airplane has an engine warnings (integer).

   an airplane event is a business event time-stamped by
   a timestamp (date & time).
   an airplane event has an aircraft id.
   an airplane event has an engine.

   a engine is a concept.
   an engine has a pressure ratio (integer).
   an engine has a rpm (integer).

*)

let airplane =
  mk_bmd_concept "airplane" (Some "business entity")
    (mk_bmd_rec [("airplane id",mk_bmd_string ());
                 ("average engine pressure ratio",mk_bmd_int ());
                 ("average engine rpm",mk_bmd_int ());
                 ("engine warnings",mk_bmd_int ());])

let airplane_event =
  mk_bmd_concept "airplane event" (Some "business event")
    (mk_bmd_rec [(* XXX ("timestamp",mk_bmd_date ()); XXX *)
       ("aircraft id",mk_bmd_string ());
       ("engine",mk_bmd_ref "engine");])

let engine =
  mk_bmd_concept "engine" None
    (mk_bmd_rec [("pressure ratio",mk_bmd_int ());
                 ("rpm",mk_bmd_int ());])

let airline_schema =
  mk_bmd_schema [airplane;airplane_event;engine;]

(**
   an account is a business entity identified by an id.
   an account has a status (an account status).
   an account status can be one of: Excellent, Good, Fair, Poor, Risky.

   a transaction is a business event time-stamped by a date.
   a transaction is related to an account.
   a transaction has an amount (a number).
   a transaction has a country code.

   an authorization response is a business event time-stamped by a date.
   an authorization response is related to an account.
   an authorization response has a message.
   an authorization response has a transaction.
*)

let account_concept =
  mk_bmd_concept "account" (Some "business entity")
    (mk_bmd_rec [("id",mk_bmd_string ());
                 ("status",mk_bmd_ref "account status");])

let account_status_concept =
  mk_bmd_concept "account status" None
    (mk_bmd_enum ["Excellent";"Good";"Fair";"Poor";"Risky";])

let transaction_concept =
  mk_bmd_concept "transaction" (Some "business event")
    (mk_bmd_rec [(* XXX ("transaction timestamp",mk_bmd_date ()); XXX *)
       ("acount",mk_bmd_ref "account");
       ("amount",mk_bmd_real ());
       ("country code",mk_bmd_string ());])

let authorization_response_concept =
  mk_bmd_concept "authorization response" (Some "business event")
    (mk_bmd_rec [(* XXX ("authorization timestamp",mk_bmd_date ()); XXX *)
       ("account",mk_bmd_ref "account");
       ("message",mk_bmd_string ());
       ("transaction",mk_bmd_string ());])

let creditcard_schema =
  mk_bmd_schema [account_concept;
                 account_status_concept;
                 transaction_concept;
                 authorization_response_concept;]
