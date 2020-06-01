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

  module Register = struct
    type t= string
    let compare= String.compare

    type content=
      | Seq of string
      | Line of string
    let compare_content t1 t2=
      match t1, t2 with
      | Seq s1, Seq s2-> String.compare s1 s2
      | Line s1, Line s2-> String.compare s1 s2
      | Seq _, Line _-> 1
      | Line _, Seq _-> -1
  end

  module RegisterMap = Map.Make(Register)

  type register= string option
  type count= int option
  type keyseq= Modal.Key.t list

  module Resolver = struct

    type t= config -> status -> keyseq -> result

    and config= {
      mode: Mode.Name.t signal;
      set_mode: ?step:step -> Mode.Name.t -> unit;
      keyseq: keyseq signal;
      set_keyseq: ?step:step -> keyseq -> unit;
      mutable resolver_insert: t;
      mutable resolver_normal: t;
      mutable resolver_visual: t;
      mutable resolver_command: t;
    }

    and status= {
      register: register;
      count: count;
    }

    and result=
      | Accept of (Edit_action.t * keyseq * Mode.Name.t)
      | Continue of (t * status * keyseq)
      | Rejected of keyseq

    let resolver_dummy= fun _config _status keyseq-> Rejected keyseq

    let resolver_insert _config _status keyseq=
      match keyseq with
      | []-> Rejected []
      | key::tl->
        if key.Key.control && key.code = Char "[" then
          Accept (
            Vi [Motion (Left, 1); ChangeMode Normal]
            , tl
            , Mode.Name.Normal)
        else if key.code = Escape then
          Accept (
            Vi [Motion (Left, 1); ChangeMode Normal]
            , tl
            , Mode.Name.Normal)
        else
          Accept (
            Bypass [key]
            , tl
            , Mode.Name.Insert)

    module Common = struct
      let get_count status=
        match status.count with
        | Some count-> count
        | None-> 1

      let get_register status=
        match status.register with
        | None | Some "\'"-> "\""
        | Some reg-> reg

      let try_count continuation config status keyseq=
        let get_count numseq=
          match numseq with
          | ""-> status.count
          | _->
            let num= int_of_string numseq in
            match status.count with
            | Some count-> Some (count * num)
            | None-> Some num
        in
        let rec other_num numseq config status keyseq=
          match keyseq with
          | []-> Rejected keyseq
          | key::tl->
            match key.Key.code with
            | Char code->
              if String.length code = 1 && code >= "0" && code <= "9"
                && not (key.Key.control || key.Key.meta || key.Key.shift)
              then
                let resolver= other_num (numseq ^ code) in
                Continue (resolver, status, tl)
              else
                continuation config { status with count= (get_count numseq) } keyseq
            | Escape-> Rejected tl
            | _->
              continuation config { status with count= get_count numseq } keyseq
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
                  Continue (resolver, status, tl)
                else
                  continuation config { status with count= get_count "" } keyseq
              else
                continuation config { status with count= get_count "" } keyseq
            | Escape-> Rejected tl
            | _->
              continuation config status keyseq
        in
        first_num ()

      let try_register next_mode continuation config status keyseq=
        let get_register _config status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char code->
                let resolver= continuation in
                Continue (resolver, { status with register= Some code }, tl)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char code->
              if code = "\"" then
                Continue (get_register, status, tl)
              else
                continuation config status keyseq
            | _-> Rejected keyseq
          else
            Accept (Bypass [key], tl, next_mode)

      let try_motion next_mode config status keyseq=
        let try_motion_g _config status keyseq=
          let count= get_count status in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "e"-> Accept (
                  Vi [Motion (Word_back_end, count)]
                  , tl
                  , next_mode)
              | Char "E"-> Accept (
                  Vi [Motion (WORD_back_end, count)]
                  , tl
                  , next_mode)
              | Char "g"-> Accept (
                  match status.count with
                  | None->
                    Vi [Motion (GotoLine_first, count)]
                    , tl
                    , next_mode
                  | Some count->
                    Vi [Motion (GotoLine, (count-1))]
                    , tl
                    , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        let try_motion_occurence ?(backward=false) _config status keyseq=
          let count= get_count status in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char chr->
                if backward then
                  Accept (
                    Vi [Motion (Occurrence_inline_back chr, count)]
                    , tl
                    , next_mode)
                else
                  Accept (
                    Vi [Motion (Occurrence_inline chr, count)]
                    , tl
                    , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        let try_motion_occurence_till ?(backward=false) _config status keyseq=
          let count= get_count status in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char chr->
                if backward then
                  Accept (
                    Vi [Motion (Occurrence_inline_till_back chr, count)]
                    , tl
                    , next_mode)
                else
                  Accept (
                    Vi [Motion (Occurrence_inline_till chr, count)]
                    , tl
                    , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        let try_motion_n _config status keyseq=
          let count= get_count status in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "h"-> Accept (
                  Vi [Motion (Left, count)]
                  , tl
                  , next_mode)
              | Char "l"-> Accept (
                  Vi [Motion (Right, count)]
                  , tl
                  , next_mode)
              | Char "j"-> Accept (
                  Vi [Motion (Downward, count)]
                  , tl
                  , next_mode)
              | Char "k"-> Accept (
                  Vi [Motion (Upward, count)]
                  , tl
                  , next_mode)
              | Char "0"-> Accept (
                  Vi [Motion (Line_FirstChar, count)]
                  , tl
                  , next_mode)
              | Char "$"-> Accept (
                  Vi [Motion (Line_LastChar, count)]
                  , tl
                  , next_mode)
              | Char "^"-> Accept (
                  Vi [Motion (Line_FirstNonBlank, count)]
                  , tl
                  , next_mode)
              | Char "w"-> Accept (
                  Vi [Motion (Word, count)]
                  , tl
                  , next_mode)
              | Char "W"-> Accept (
                  Vi [Motion (WORD, count)]
                  , tl
                  , next_mode)
              | Char "b"-> Accept (
                  Vi [Motion (Word_back, count)]
                  , tl
                  , next_mode)
              | Char "B"-> Accept (
                  Vi [Motion (WORD_back, count)]
                  , tl
                  , next_mode)
              | Char "e"-> Accept (
                  Vi [Motion (Word_end, count)]
                  , tl
                  , next_mode)
              | Char "E"-> Accept (
                  Vi [Motion (WORD_end, count)]
                  , tl
                  , next_mode)
              | Char "G"-> Accept (
                  match status.count with
                  | None->
                    Vi [Motion (GotoLine_last, count)]
                    , tl
                    , next_mode
                  | Some count->
                    Vi [Motion (GotoLine, (count-1))]
                    , tl
                    , next_mode)
              | Char "g"->
                let resolver= try_motion_g in
                Continue (resolver, status, tl)
              | Char "f"->
                let backward= false in
                let resolver= try_motion_occurence ~backward in
                Continue (resolver, status, tl)
              | Char "F"->
                let backward= true in
                let resolver= try_motion_occurence ~backward in
                Continue (resolver, status, tl)
              | Char "t"->
                let backward= false in
                let resolver= try_motion_occurence_till ~backward in
                Continue (resolver, status, tl)
              | Char "T"->
                let backward= true in
                let resolver= try_motion_occurence_till ~backward in
                Continue (resolver, status, tl)
              | Char "%"-> Accept (
                  Vi [Motion (Match, 1)]
                  , tl
                  , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        try_count try_motion_n config status keyseq
    end

    module Normal = struct
      let try_change_mode _config _status keyseq=
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "i"->
              Accept (
                Vi [ChangeMode Insert]
                , tl
                , Mode.Name.Insert)
            | Char "I"->
              Accept (
                Vi [
                  Motion (Line_FirstNonBlank, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert)
            | Char "a"->
              Accept (
                Vi [
                  Motion (Right_nl, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert)
            | Char "A"->
              Accept (
                Vi [
                  Motion (Line_LastChar_nl, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert)
            | Char "v"->
              Accept (
                Vi [ ChangeMode Visual]
                , tl
                , Mode.Name.Visual)
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_modify config status keyseq=
        let open Common in
        let try_motion_n
            ~action
            _config status keyseq
          =
          let next_mode=
            if action = `Change
            then Mode.Name.Insert
            else Mode.Name.Normal
          in
          let make_actions status tl motion=
            let action=
              let register= get_register status
              and count= get_count status in
              match action with
              | `Change-> Change (register, motion, count)
              | `Delete-> Delete (register, motion, count)
              | `Yank-> Yank (register, motion, count)
            in
            Accept (
              Vi [action]
              , tl
              , next_mode)
          in
          let try_motion_g _config status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char "e"-> make_actions status tl Word_back_end
                | Char "E"-> make_actions status tl WORD_back_end
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_quote ?(inner=false) _config status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char chr->
                  if inner then
                    make_actions status tl (Quote_inner chr)
                  else
                    make_actions status tl (Quote_include chr)
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_object ?(inner=false) _config status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char "(" | Char ")" | Char "b"->
                  if inner then
                    make_actions status tl Parenthesis_inner
                  else
                    make_actions status tl Parenthesis_include
                | Char "[" | Char "]"->
                  if inner then
                    make_actions status tl Bracket_inner
                  else
                    make_actions status tl Bracket_include
                | Char "<" | Char ">"->
                  if inner then
                    make_actions status tl AngleBracket_inner
                  else
                    make_actions status tl AngleBracket_include
                | Char "{" | Char "}"->
                  if inner then
                    make_actions status tl Brace_inner
                  else
                    make_actions status tl Brace_include
                | Char "'"->
                  if inner then
                    make_actions status tl (Quote_inner "'")
                  else
                    make_actions status tl (Quote_include "'")
                | Char "\""->
                  if inner then
                    make_actions status tl (Quote_inner "\"")
                  else
                    make_actions status tl (Quote_include "\"")
                | Char "w"->
                  if inner then
                    make_actions status tl Word_inner
                  else
                    make_actions status tl Word_include
                | Char "W"->
                  if inner then
                    make_actions status tl WORD_inner
                  else
                    make_actions status tl WORD_include
                | Char "q"->
                  let resolver= try_motion_quote ~inner in
                  Continue (resolver, status, tl)
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_occurence ?(backward=false) _config _status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char chr->
                  if backward then
                    make_actions status
                      tl
                      (Occurrence_inline_back chr)
                  else
                    make_actions status tl (Occurrence_inline chr)
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_occurence_till ?(backward=false) _config status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char chr->
                  if backward then
                    make_actions status
                      tl
                      (Occurrence_inline_till_back chr)
                  else
                    make_actions status tl (Occurrence_inline_till chr)
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "h"-> make_actions status tl Left
              | Char "l"-> make_actions status tl Right
              | Char "j"-> make_actions status tl Downward
              | Char "k"-> make_actions status tl Upward
              | Char "0"-> make_actions status tl Line_FirstChar
              | Char "$"-> make_actions status tl Line_LastChar
              | Char "^"-> make_actions status tl Line_FirstNonBlank
              | Char "w"-> make_actions status tl Word
              | Char "W"-> make_actions status tl WORD
              | Char "b"-> make_actions status tl Word_back
              | Char "B"-> make_actions status tl WORD_back
              | Char "e"-> make_actions status tl Word_end
              | Char "E"-> make_actions status tl WORD_end
              | Char "g"->
                let resolver= try_motion_g in
                Continue (resolver, status, tl)
              | Char "d"->
                if action = `Delete then
                  make_actions status tl Line
                else Rejected keyseq
              | Char "a"->
                let inner= false in
                let resolver= try_motion_object ~inner in
                Continue (resolver, status, tl)
              | Char "i"->
                let inner= true in
                let resolver= try_motion_object ~inner in
                Continue (resolver, status, tl)
              | Char "f"->
                let backward= false in
                let resolver= try_motion_occurence ~backward in
                Continue (resolver, status, tl)
              | Char "F"->
                let backward= true in
                let resolver= try_motion_occurence ~backward in
                Continue (resolver, status, tl)
              | Char "t"->
                let backward= false in
                let resolver= try_motion_occurence_till ~backward in
                Continue (resolver, status, tl)
              | Char "T"->
                let backward= true in
                let resolver= try_motion_occurence_till ~backward in
                Continue (resolver, status, tl)
              | Char "%"-> make_actions status tl Match
              | Char "y"->
                if action = `Yank then
                  make_actions status tl Line
                else Rejected keyseq
              | _->
                Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        let determin _config status keyseq=
          let count= get_count status
          and register= get_register status in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "u"-> Accept (Vi [Undo count], tl, Mode.Name.Normal)
              | Char "p"-> Accept (Vi [Paste_after (register, count)], tl, Mode.Name.Normal)
              | Char "P"-> Accept (Vi [Paste_before (register, count)], tl, Mode.Name.Normal)
              | Char "d"->
                let resolver= Common.try_count
                  (try_motion_n ~action:`Delete) in
                Continue (resolver, status, tl)
              | Char "c"->
                let resolver= Common.try_count
                  (try_motion_n ~action:`Change) in
                Continue (resolver, status, tl)
              | Char "D"-> Accept (
                  Vi [Delete (register, Line_LastChar, count)],
                  tl,
                  Mode.Name.Normal)
              | Char "C"-> Accept (
                  Vi [Delete (register, Line_LastChar, count)],
                  tl,
                  Mode.Name.Insert)
              | Char "x"->
                Accept (
                  Vi [Delete (register, Right, count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "s"->
                Accept (
                  Vi [Delete (register, Right, count)]
                  , tl
                  , Mode.Name.Insert)
              | Char "J"->
                Accept (
                  Vi [(Join count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "y"->
                let resolver= Common.try_count
                  (try_motion_n ~action:`Yank) in
                Continue (resolver, status, tl)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        determin config status keyseq

      let try_insert _config status keyseq=
        let count= Common.get_count status in
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

      let try_motion_modify_insert config status keyseq=
        match Common.try_motion Mode.Name.Normal config status keyseq with
        | Rejected keyseq->
          let resolver config status keyseq=
            match try_modify config status keyseq with
            | Rejected keyseq->
              let resolver= try_insert in
              Continue (resolver, status, keyseq)
            | r-> r
          in
          Continue (resolver, status, keyseq)
        | r-> r

      let resolver_normal config status keyseq=
        match keyseq with
        | []-> Rejected []
        | _->
          match try_change_mode config status keyseq with
          | Rejected keyseq->
            Common.try_register Mode.Name.Normal (Common.try_count (Common.try_register Mode.Name.Normal
              try_motion_modify_insert))
              config status keyseq
          | r-> r

    end

    module Visual = struct
      let try_change_mode _config _status keyseq=
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if key.Key.control && key.code = Char "[" then
            Accept (
              Vi [ChangeMode Normal]
              , tl
              , Mode.Name.Normal)
          else if key.code = Escape then
            Accept (
              Vi [ChangeMode Normal]
              , tl
              , Mode.Name.Normal)
          else
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "v"->
              Accept (
                Vi [ ChangeMode Normal]
                , tl
                , Mode.Name.Normal)
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_motion= Common.try_motion Mode.Name.Visual

      let try_modify _config status keyseq=
        let register= Common.get_register status in
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "c" | Char "s"->
              Accept (
                Vi [ DeleteSelected register; ChangeMode Insert]
                , tl
                , Mode.Name.Insert)
            | Char "d" | Char "x"->
              Accept (
                Vi [ DeleteSelected register; ChangeMode Normal ]
                , tl
                , Mode.Name.Normal)
            | Char "y"->
              Accept (
                Vi [ YankSelected register; ChangeMode Normal ]
                , tl
                , Mode.Name.Normal)
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_motion_modify config status keyseq=
        match try_motion config status keyseq with
        | Rejected keyseq->
          Continue (try_modify, status, keyseq)
        | r-> r

      let resolver_visual config status keyseq=
        match keyseq with
        | []-> Rejected []
        | _->
          match try_change_mode config status keyseq with
          | Rejected keyseq->
            Common.try_register Mode.Name.Visual
              (Common.try_count try_motion_modify)
              config status keyseq
          | r-> r

    end

    let make_config
        ?(mode= Mode.Name.Insert)
        ?(keyseq=[])
        ?(resolver_insert= resolver_insert)
        ?(resolver_normal= Normal.resolver_normal)
        ?(resolver_visual= Visual.resolver_visual)
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
        resolver_visual;
        resolver_command;
      }

    let rec interpret
      ?resolver ?(keyseq=[])
      config
      status
      (keyIn: Modal.Key.t MsgBox.t) (action: Edit_action.t MsgBox.t) ()
      =
      let resolver=
        match resolver with
        | Some resolver-> resolver
        | None-> match S.value config.mode with
          | Mode.Name.Insert-> config.resolver_insert
          | Mode.Name.Visual-> config.resolver_visual
          | _-> config.resolver_normal
      in
      (match keyseq with
      | []-> MsgBox.get keyIn >>= fun key-> Thread.return [key]
      | _-> Thread.return keyseq)
      >>= fun keyseq->
        match resolver config status keyseq with
        | Accept (edit, keyseq, next_mode)->
          config.set_mode next_mode;
          MsgBox.put action edit >>=
          interpret config { status with count= None} ~keyseq keyIn action
        | Continue (resolver, status, keyseq)->
          interpret config status ~resolver ~keyseq keyIn action ()
        | Rejected _keyseq->
          MsgBox.put action Dummy >>=
          interpret config { status with count= None } keyIn action
  end
end

