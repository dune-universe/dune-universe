(*
 * interpret.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


open Vi_action
open Edit_action
open React

module Make (Concurrent:Mew.Concurrent.S) =
struct
  module MsgBox = Concurrent.MsgBox
  module Thread = Concurrent.Thread

  let (>>=)= Thread.bind

  type keyseq= Modal.Key.t list

  module Resolver = struct
    type t= status -> keyseq -> result

    and result=
      | Accept of (Edit_action.t * keyseq * Mode.Name.t)
      | Continue of (t * keyseq)
      | Rejected of keyseq

    and status= {
      mode: Mode.Name.t signal;
      set_mode: ?step:step -> Mode.Name.t -> unit;
      keyseq: keyseq signal;
      set_keyseq: ?step:step -> keyseq -> unit;
      mutable resolver_insert: t;
      mutable resolver_normal: t;
      mutable resolver_command: t;
    }

    let resolver_dummy= fun _status keyseq-> Rejected keyseq

    let resolver_insert status keyseq=
      match keyseq with
      | []-> Rejected []
      | key::tl->
        if key.Key.control && key.code = Char "[" then
          (status.set_mode Mode.Name.Normal;
          Accept (
            Vi [Motion ((Left 1), 1); ChangeMode Normal]
            , tl
            , Mode.Name.Normal))
        else if key.code = Escape then
          (status.set_mode Mode.Name.Normal;
          Accept (
            Vi [Motion ((Left 1), 1); ChangeMode Normal]
            , tl
            , Mode.Name.Normal))
        else
          Accept (
            Bypass [key]
            , tl
            , Mode.Name.Insert)

    module Normal = struct
      let try_count continuation status keyseq=
        let get_count numseq=
          match numseq with
          | ""-> None
          | _-> Some (int_of_string numseq)

        in
        let rec other_num numseq _status keyseq=
          match keyseq with
          | []-> Rejected keyseq
          | key::tl->
            match key.Key.code with
            | Char code->
              if String.length code = 1 && code >= "0" && code <= "9"
                && not (key.Key.control || key.Key.meta || key.Key.shift)
              then
                let resolver= other_num (numseq ^ code) in
                Continue (resolver, tl)
              else
                continuation (get_count numseq) status keyseq
            | Escape-> Rejected tl
            | _->
              continuation (get_count numseq) status keyseq
        in
        let first_num ()=
          match keyseq with
          | []-> Rejected keyseq
          | key::tl->
            match key.Key.code with
            | Char code->
              if String.length code = 1 && not (key.Key.control || key.Key.meta || key.Key.shift) then
                if code >= "1" && code <= "9" then
                  let resolver= other_num code in
                  Continue (resolver, tl)
                else
                  continuation (get_count "") status keyseq
              else
                continuation (get_count "") status keyseq
            | Escape-> Rejected tl
            | _->
              continuation (get_count "") status keyseq
        in
        first_num ()

      let try_motion count=
        let count=
          match count with
          | Some count-> count
          | None-> 1
        in
        let try_motion_g count num _status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "e"-> Accept (
                  Vi [Motion ((Word_back_end num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "E"-> Accept (
                  Vi [Motion ((WORD_back_end num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "g"-> Accept (
                  Vi [Motion (GotoLine_first, count)]
                  , tl
                  , Mode.Name.Normal)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        let try_motion_n num _status keyseq=
          let num= match num with
            | Some n-> n
            | None-> 1
          in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "h"-> Accept (
                  Vi [Motion ((Left num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "l"-> Accept (
                  Vi [Motion ((Right num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "j"-> Accept (
                  Vi [Motion ((Downward num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "k"-> Accept (
                  Vi [Motion ((Upward num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "0"-> Accept (
                  Vi [Motion ((Line_FirstChar num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "$"-> Accept (
                  Vi [Motion ((Line_LastChar num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "w"-> Accept (
                  Vi [Motion ((Word num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "b"-> Accept (
                  Vi [Motion ((Word_back num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "e"-> Accept (
                  Vi [Motion ((Word_end num), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "G"-> Accept (
                  Vi [Motion (GotoLine_last, count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "g"->
                let resolver= try_motion_g count num in
                Continue (resolver, tl)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        try_count try_motion_n

      let try_change_mode status keyseq=
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "i"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | Char "I"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_FirstNonBlank 1, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | Char "a"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Right_nl 1, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | Char "A"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_LastChar_nl 1, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_modify count=
        let count=
          match count with
          | Some count-> count
          | None->1
        in
        let try_motion_n
            ?(change=false)
            ?(d=false)
            count num _status keyseq
          =
          let num= match num with
            | Some n-> n
            | None-> 1
          and next_mode=
            if change
            then Mode.Name.Insert
            else Mode.Name.Normal
          in
          let make_actions tl motion count=
            let action=
              if change
              then Change (motion, count)
              else Delete (motion, count)
            in
            Accept (
              Vi [action]
              , tl
              , next_mode)
          in
          let try_motion_g count num _status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char "e"-> make_actions tl (Word_back_end num) count
                | Char "E"-> make_actions tl (WORD_back_end num) count
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "h"-> make_actions tl (Left num) count
              | Char "l"-> make_actions tl (Right num) count
              | Char "j"-> make_actions tl (Downward num) count
              | Char "k"-> make_actions tl (Upward num) count
              | Char "0"-> make_actions tl (Line_FirstChar num) count
              | Char "$"-> make_actions tl (Line_LastChar num) count
              | Char "w"-> make_actions tl (Word num) count
              | Char "b"-> make_actions tl (Word_back num) count
              | Char "e"-> make_actions tl (Word_end num) count
              | Char "g"->
                let resolver= try_motion_g count num in
                Continue (resolver, tl)
              | Char "d"-> if d then
                  make_actions tl Line count
                else Rejected keyseq
              | _->
                Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        let determin count _status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "u"-> Accept (Vi [Undo count], tl, Mode.Name.Normal)
              | Char "p"-> Accept (Vi [Paste count], tl, Mode.Name.Normal)
              | Char "d"->
                let resolver= try_count (try_motion_n ~d:true count) in
                Continue (resolver, tl)
              | Char "c"->
                let change= true in
                let resolver= try_count (try_motion_n ~change count) in
                Continue (resolver, tl)
              | Char "x"->
                Accept (
                  Vi [Delete ((Right 1), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "s"->
                Accept (
                  Vi [Delete ((Right 1), count)]
                  , tl
                  , Mode.Name.Insert)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        determin count

      let try_insert count _status keyseq=
        let count=
          match count with
          | Some count-> count
          | None-> 1
        in
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "o"-> Accept (
                Vi [Insert ((Newline_below ""), count)]
                , tl
                , Mode.Name.Insert)
            | Char "O"-> Accept (
                Vi [Insert ((Newline_above ""), count)]
                , tl
                , Mode.Name.Insert)
            | _-> Rejected keyseq
          else
            Accept (Bypass [key], tl, Mode.Name.Normal)

      let try_motion_modify_insert count status keyseq=
        match try_motion count status keyseq with
        | Rejected keyseq->
          let resolver status keyseq=
            match try_modify count status keyseq with
            | Rejected keyseq->
              let resolver= try_insert count in
              Continue (resolver, keyseq)
            | r-> r
          in
          Continue (resolver, keyseq)
        | r-> r

      let resolver_normal status keyseq=
        match keyseq with
        | []-> Rejected []
        | _->
          match try_change_mode status keyseq with
          | Rejected keyseq->
            try_count try_motion_modify_insert status keyseq
          | r-> r

    end

    let make_status
        ?(mode= Mode.Name.Insert)
        ?(keyseq=[])
        ?(resolver_insert= resolver_insert)
        ?(resolver_normal= Normal.resolver_normal)
        ?(resolver_command= resolver_dummy)
        ()
      =
      let mode, set_mode= React.S.create mode in
      let keyseq, set_keyseq= React.S.create keyseq in
      {
        mode;
        set_mode;
        keyseq;
        set_keyseq;
        resolver_insert;
        resolver_normal;
        resolver_command;
      }

    let rec interpret
      ?resolver ?(keyseq=[])
      status
      (keyIn: Modal.Key.t MsgBox.t) (action: Edit_action.t MsgBox.t) ()
      =
      let resolver=
        match resolver with
        | Some resolver-> resolver
        | None-> match S.value status.mode with
          | Mode.Name.Insert-> status.resolver_insert
          | _-> status.resolver_normal
      in
      (match keyseq with
      | []-> MsgBox.get keyIn >>= fun key-> Thread.return [key]
      | _-> Thread.return keyseq)
      >>= fun keyseq->
        match resolver status keyseq with
        | Accept (edit, keyseq, next_mode)->
          status.set_mode next_mode;
          MsgBox.put action edit >>=
          interpret status ~keyseq keyIn action
        | Continue (resolver, keyseq)->
          interpret status ~resolver ~keyseq keyIn action ()
        | Rejected _keyseq->
          MsgBox.put action Dummy >>=
          interpret status keyIn action
  end
end

