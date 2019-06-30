(*
 * Copyright 2019 Fabien Lheureux <fabien.lheureux@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *)

let slavo_Germanic sstring =
  let len = String.length sstring in
  let rec loop i =
    if i = len then false
    else
      match String.unsafe_get sstring i with
      | 'W' | 'K' -> true
      | 'C' ->
         if i + 1 < len then
           if String.unsafe_get sstring (i + 1) = 'Z' then true else loop (i+1)
         else false
      | _ -> loop (i + 1)
  in
  loop 0

let is_vowel sstring pos =
  if pos < 0 then false
  else match String.get sstring pos with
       | 'A' | 'E' | 'I' | 'O' | 'U' | 'Y' -> true
       | _ -> false

let string_at sstring start length list =
  start >= 0
  && begin
      let len1 = String.length sstring in
      start < len1
      && begin
          Array.exists
            begin fun s2 ->
            let rec loop i1 i2 =
              i2 = length
              || (i1 <> len1
                  && String.unsafe_get sstring i1 = String.unsafe_get s2 i2
                  && loop (i1 + 1) (i2 + 1))
            in loop start 0
            end
            list
        end
    end

let double_metaphone sstring =
  let primary   = ref "" in
  let secondary = ref "" in
  let current   = ref 0 in

  let length   = String.length (sstring) in
  let last     = length - 1 in
  let original = sstring ^ "     " in

  let original = String.uppercase_ascii original in

(*    // skip this at beginning of word*)

  if (string_at original 0 2 [|"GN"; "KN"; "PN"; "WR"; "PS"|]) then incr current;

(*    // Initial "X" is pronounced "Z" e.g. "Xavier"*)

    if (String.sub original 0 1 = "X") then
    begin
      primary   := !primary ^ "S";  (* // "Z" maps to "S"*)
      secondary := !secondary ^ "S";
      incr current;
    end;

(try
(*    // main loop*)
    while (String.length !primary < 4 || String.length !secondary < 4) do
      if (!current >= length) then raise Exit;

      begin
(try
      match (String.sub original !current 1) with
      | "A"
      | "E"
      | "I"
      | "O"
      | "U"
      | "Y" ->
          if (!current = 0) then begin
            (*// all init vowels now map to "A"*)
            primary   := !primary ^ "A";
            secondary := !secondary ^ "A";
          end;
          incr current;

      | "B" ->
(*          // "-mb", e.g. "dumb", already skipped over ...*)
          primary   := !primary ^ "P";
          secondary := !secondary ^ "P";

          if String.sub original (!current + 1) 1 = "B" then
            begin incr current; incr current; end
          else  incr current;


        | "Ç" ->
          primary   := !primary ^ "S";
          secondary := !secondary ^ "S";

        | "C" ->
(*          // various gremanic*)
          if ((!current > 1)
              && not (is_vowel original (!current - 2))
              && string_at original (!current - 1) 3 [| "ACH" |]
              && ((String.sub original (!current + 2)  1 <> "I")
                  && ((String.sub original (!current + 2)  1 <> "E")
                      || string_at original (!current - 2) 6 [|"BACHER"; "MACHER"|]))) then begin
          primary   := !primary ^ "K";
          secondary := !secondary ^ "K";
          incr current;
          incr current;
          raise Exit
          end;

(*          // special case "caesar"*)
          if ((!current = 0)
              && string_at original !current 6 [|"CAESAR"|]) then begin
          primary   := !primary ^ "S";
          secondary := !secondary ^ "S";
          incr current;
          incr current;
          raise Exit
          end;

(*          // italian "chianti"*)
          if (string_at original !current 4 [|"CHIA"|]) then begin
          primary   := !primary ^ "K";
          secondary := !secondary ^ "K";
          incr current;
          incr current;
          raise Exit
          end;

          if (string_at original !current 2 [|"CH"|]) then begin

(*            // find "michael"*)
            if ((!current > 0)  && string_at original !current 4 [|"CHAE"|]) then begin
              primary   := !primary ^ "K";
              secondary := !secondary ^ "K";
              incr current;
              incr current;
              raise Exit
            end;

(*            // greek roots e.g. "chemistry", "chorus"*)
            if ((!current = 0)
                && (string_at original (!current + 1) 5 [|"HARAC"; "HARIS"|])
                    || string_at original (!current + 1) 3 [|"HOR"; "HYM"; "HIA"; "HEM"|])
                && not (string_at original 0 5 [|"CHORE"|]) then begin
                     primary   := !primary ^ "K";
                     secondary := !secondary ^ "K";
                     incr current;
                     incr current;
                     raise Exit
                end;

(*            // germanic, greek, or otherwise "ch" for "kh" sound*)
            if ((string_at original 0 4 [|"VAN "; "VON "|])
                 || string_at original 0 3 [|"SCH"|])
(*                // "architect" but not "arch", orchestra, "orchid"*)
                || string_at original (!current - 2) 6 [|"ORCHES"; "ARCHIT"; "ORCHID"|]
                || string_at original (!current + 2) 1 [|"T"; "S"|]
                || ((string_at original (!current - 1) 1 [|"A";"O";"U";"E"|])
                     || (!current = 0))
(*                    // e.g. "wachtler", "weschsler", but not "tichner"*)
                    && string_at original (!current + 2) 1 [|"L";"R";"N";"M";"B";"H";"F";"V";"W";" "|] then begin
                      primary   := !primary ^ "K";
                      secondary := !secondary ^ "K";
                    end
            else begin
              if (!current > 0) then begin
                if string_at original 0 2 [|"MC"|] then begin
(*                  // e.g. "McHugh"*)
                  primary := !primary ^ "K";
                  secondary := !secondary ^ "K";
                end else begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "K";
                end
              end else begin
                primary := !primary ^ "X";
                secondary := !secondary ^ "X";
              end
            end;
            incr current; incr current;
            raise Exit
          end;

(*          // e.g. "czerny"*)
          if string_at original !current 2 [|"CZ"|] &&
            not (string_at original (!current - 2) 4 [|"WICZ"|]) then begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "X";
            incr current; incr current;
            raise Exit
            end;

(*          // e.g. "focaccia"*)
          if string_at original (!current + 1) 3 [|"CIA"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "X";
            incr current; incr current; incr current;
            raise Exit
          end;
(*          // double "C", but not McClellan*)
          if string_at original !current 2 [|"CC"|]
              && not ((!current = 1)
                   && (String.sub original 0 1) = "M") then begin
(*            // "bellocchio" but not "bacchus"*)
            if string_at original (!current + 2) 1                       [|"I";"E";"H"|]
                && not (string_at original (!current + 2) 2                          [|"HU"|]) then begin
(*              // "accident", "accede", "succeed"*)
              if (((!current = 1)
                   && (String.sub original (!current - 1) 1) = "A"))
                  || string_at original (!current - 1) 5                            [|"UCCEE"; "UCCES"|] then begin
                primary := !primary ^ "KS";
                secondary := !secondary ^ "KS";
(*                // "bacci", "bertucci", other italian*)
              end else begin
                primary := !primary ^ "X";
                secondary := !secondary ^ "X";
              end;
              incr current; incr current; incr current;
              raise Exit
            end else begin
(*              // Pierce's rule*)
              primary := !primary ^ "K";
              secondary := !secondary ^ "K";
              incr current; incr current;
              raise Exit
            end
              end;

          if string_at original  !current 2 [|"CK";"CG";"CQ"|] then begin
            primary := !primary ^ "K";
            secondary := !secondary ^ "K";
            incr current; incr current;
            raise Exit
          end;

          if string_at original  !current 2 [|"CI";"CE";"CY"|] then begin
(*            // italian vs. english*)
            if string_at original  !current 3  [|"CIO";"CIE";"CIA"|] then begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "X";
            end else begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "S";
            end;
            incr current; incr current;
            raise Exit
          end;

(*          // else*)
          primary := !primary ^ "K";
          secondary := !secondary ^ "K";


(*          // name sent in "mac caffrey", "mac gregor"*)
          if string_at original  (!current + 1) 2 [|" C";" Q";" G"|] then begin
            incr current; incr current; incr current;
          end else begin
            if string_at original  (!current + 1) 1  [|"C";"K";"Q"|]
                && not (string_at original (!current + 1) 2 [|"CE";"CI"|]) then begin
              incr current; incr current;
            end else begin
              incr current;
            end
          end;
          raise Exit;

        | "D" ->
          if string_at original  !current 2    [|"DG"|] then begin
            if string_at original  (!current + 2) 1          [|"I";"E";"Y"|] then begin
(*              // e.g. "edge"*)
              primary := !primary ^ "J";
              secondary := !secondary ^ "J";
              incr current; incr current; incr current;
              raise Exit
            end else begin
(*              // e.g. "edgar"*)
              primary := !primary ^ "TK";
              secondary := !secondary ^ "TK";
              incr current; incr current;
              raise Exit
            end
          end;

          if string_at original  !current 2   [|"DT";"DD"|] then begin
            primary := !primary ^ "T";
            secondary := !secondary ^ "T";
            incr current; incr current;
            raise Exit
          end;

(*          // else*)
          primary := !primary ^ "T";
          secondary := !secondary ^ "T";
          incr current;
          raise Exit

        | "F" ->
          if (String.sub original (!current + 1) 1) = "F" then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "F";
          secondary := !secondary ^ "F";
          raise Exit

        | "G" ->
          if (String.sub original (!current + 1) 1) = "H" then begin
            if (!current > 0)
                && not (is_vowel original (!current - 1)) then begin
              primary := !primary ^ "K";
              secondary := !secondary ^ "K";
              incr current; incr current;
              raise Exit
            end;

            if (!current < 3) then begin
(*              // "ghislane", "ghiradelli"*)
              if (!current = 0) then begin
                if (String.sub original (!current + 2) 1) = "I" then begin
                  primary := !primary ^ "J";
                  secondary := !secondary ^ "J";
                end else begin
                  primary := !primary ^ "K";
                  secondary := !secondary ^ "K";
                end;
                incr current; incr current;
                raise Exit
              end
            end;

(*            // Parker's rule (with some further refinements) - e.g. "hugh"*)
            if (((!current > 1)
                 && string_at original (!current - 2) 1                  [|"B";"H";"D"|])
(*                // e.g. "bough"*)
                || ((!current > 2)
                    && string_at original (!current - 3) 1                 [|"B";"H";"D"|])
(*                // e.g. "broughton"*)
                || ((!current > 3)
                    && string_at original (!current - 4) 1    [|"B";"H"|])) then begin
              incr current; incr current;
              raise Exit
            end else begin
(*              // e.g. "laugh", "McLaughlin", "cough", "gough", "rough", "tough"*)
              if ((!current > 2)
                  && (String.sub original (!current - 1) 1) = "U")
                  && string_at original (!current - 3) 1  [|"C";"G";"L";"R";"T"|] then begin
                primary := !primary ^ "F";
                secondary := !secondary ^ "F";
              end else if (!current > 0)
                        && String.sub original (!current - 1) 1 <> "I" then begin
                primary := !primary ^ "K";
                secondary := !secondary ^ "K";
              end;
              incr current; incr current;
              raise Exit
            end
          end;

          if (String.sub original (!current + 1) 1) = "N" then begin
            if ((!current = 1) && is_vowel original 0
                && not (slavo_Germanic original)) then begin
              primary := !primary ^ "KN";
              secondary := !secondary ^ "N";
            end else begin
(*              // not e.g. "cagney"*)
              if not (string_at original (!current + 2) 2  [|"EY"|])
                  && (String.sub original (!current + 1) (String.length original - (!current + 1))) <> "Y"
                  && not (slavo_Germanic original) then begin
                 primary := !primary ^ "N";
                 secondary := !secondary ^ "KN";
              end else begin
                 primary := !primary ^ "KN";
                 secondary := !secondary ^ "KN";
              end
            end;
            incr current; incr current;
            raise Exit
          end;

(*          // "tagliaro"*)
          if string_at original  (!current + 1) 2 [|"LI"|]
              && not (slavo_Germanic original) then begin
            primary := !primary ^ "KL";
            secondary := !secondary ^ "L";
            incr current; incr current;
            raise Exit
          end ;

(*          // -ges- -gep- -gel- at beginning*)
          if ((!current = 0)
              && ((String.sub original (!current + 1) 1) = "Y"
                  || string_at original (!current + 1) 2 [|"ES";"EP";"EB";"EL";"EY";"IB";"IL";"IN";"IE";"EI";"ER"|])) then begin
            primary := !primary ^ "K";
            secondary := !secondary ^ "J";
            incr current; incr current;
            raise Exit
          end;

(*          // -ger- -gy-*)
          if (string_at original  (!current + 1) 2 [|"ER"|]
               || (String.sub original (!current + 1) 1) = "Y")
              && not (string_at original 0 6 [|"DANGER";"RANGER";"MANGER"|])
              && not (string_at original (!current - 1) 1 [|"E"; "I"|])
              && not (string_at original (!current - 1) 3 [|"RGY";"OGY"|]) then begin
            primary := !primary ^ "K";
            secondary := !secondary ^ "J";
            incr current; incr current;
            raise Exit
                         end;

(*          // italian e.g. "biaggi"*)
          if string_at original  (!current + 1) 1  [|"E";"I";"Y"|]
              || string_at original (!current - 1) 4 [|"AGGI";"OGGI"|] then begin
(*            // obvious germanic*)
            if (string_at original  0 4 [|"VAN "; "VON "|])
                 || string_at original 0 3 [|"SCH"|]
                || string_at original (!current + 1) 2
                          [|"ET"|] then begin
              primary := !primary ^ "K";
              secondary := !secondary ^ "K";
            end else begin
(*              // always soft if french ending*)
              if string_at original  (!current + 1) 4 [|"IER "|] then begin
                primary := !primary ^ "J";
                secondary := !secondary ^ "J";
              end else begin
                primary := !primary ^ "J";
                secondary := !secondary ^ "K";
              end
            end;
            incr current; incr current;
            raise Exit
                        end;

          if (String.sub original (!current + 1) 1) = "G" then begin
            incr current; incr current;
            end
          else
            incr current;

          primary := !primary ^ "K";
          secondary := !secondary ^ "K";
          raise Exit

        | "H" ->
(*          // only keep if first & before vowel or btw. 2 vowels*)
          if (((!current = 0) ||
               is_vowel original (!current - 1))
              && is_vowel original (!current + 1)) then begin
            primary := !primary ^ "H";
            secondary := !secondary ^ "H";
            incr current; incr current;
          end else
            incr current;
          raise Exit

        | "J" ->
          (*  obvious spanish "jose" "san jacinto" *)
          if string_at original  !current 4 [|"JOSE"|]             (* peut etre un not *)
              || string_at original 0 4 [|"SAN "|] then begin
            if (((!current = 0)
                 && (String.sub original (!current + 4) 1) = " "))
                || string_at original 0 4 [|"SAN "|] then begin
              primary := !primary ^ "H";
              secondary := !secondary ^ "H";
            end else begin
              primary := !primary ^ "J";
              secondary := !secondary ^ "H";
            end;
            incr current;
            raise Exit
              end;

          if (!current = 0)
              && not (string_at original !current 4 [|"JOSE"|]) then begin
            primary := !primary ^ "J";  (*  Yankelovich/Jankelowicz *)
            secondary := !secondary ^ "A";
          end else begin
            (*  spanish pron. of .e.g. "bajador" *)
            if (is_vowel original (!current - 1)
                && not (slavo_Germanic original)
                && ((String.sub original (!current + 1) 1) = "A")
                    || (String.sub original (!current + 1) 1) = "O") then begin
              primary := !primary ^ "J";
              secondary := !secondary ^ "H";
            end else begin
              if (!current = last) then begin
                primary := !primary ^ "J";
                secondary := !secondary ^ "";
              end else begin
                if not (string_at original (!current + 1) 1 [|"L";"T";"K";"S";"N";"M";"B";"Z"|])
                    && not (string_at original (!current - 1) 1 [|"S";"K";"L"|]) then begin
                  primary := !primary ^ "J";
                  secondary := !secondary ^ "J";
                end
              end
            end
          end;

          if (String.sub original (!current + 1) 1) = "J" then begin (*  it could happen *)
            incr current; incr current; end
          else
            incr current;
          raise Exit

        | "K" ->
          if (String.sub original (!current + 1) 1) = "K" then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "K";
          secondary := !secondary ^ "K";
          raise Exit

        | "L" ->
          if (String.sub original (!current + 1) 1) = "L" then begin
            (*  spanish e.g. "cabrillo" "gallegos" *)
            if ((!current = (length - 3))
                 && string_at original (!current - 1) 4
                           [|"ILLO";"ILLA";"ALLE"|]
                || (string_at original  (last-1) 2
                            [|"AS";"OS"|])
                  || string_at original last 1
                            [|"A";"O"|]
                 && string_at original (!current - 1) 4
                           [|"ALLE"|]) then begin
              primary := !primary ^ "L";
              secondary := !secondary ^ "";
              incr current; incr current;
              raise Exit
            end;
            incr current; incr current;
          end else
            incr current;
          primary := !primary ^ "L";
          secondary := !secondary ^ "L";
          raise Exit

        | "M" ->
          if (string_at original  (!current - 1) 3
                     [|"UMB"|])
               && ((((!current + 1)) = last)
                   || string_at original (!current + 2) 2
                            [|"ER"|])
              (*  "dumb" "thumb" *)
              || (String.sub original (!current + 1) 1) = "M" then begin
              incr current; incr current;
          end else begin
              incr current;
          end;
          primary := !primary ^ "M";
          secondary := !secondary ^ "M";
          raise Exit

        | "N" ->
          if (String.sub original (!current + 1) 1) = "N" then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "N";
          secondary := !secondary ^ "N";
          raise Exit

        | "Ñ" ->
          incr current;
          primary := !primary ^ "N";
          secondary := !secondary ^ "N";
          raise Exit

        | "P" ->
          if (String.sub original (!current + 1) 1) = "H" then begin
            incr current; incr current;
            primary := !primary ^ "F";
            secondary := !secondary ^ "F";
            raise Exit
          end;

          (*  also account for "campbell" and "raspberry" *)
          if string_at original  (!current + 1) 1
                     [|"P";"B"|] then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "P";
          secondary := !secondary ^ "P";
          raise Exit

        | "Q" ->
          if (String.sub original (!current + 1) 1) = "Q" then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "K";
          secondary := !secondary ^ "K";
          raise Exit

        | "R" ->
          (*  french e.g. "rogier" but exclude "hochmeier" *)
          if ((!current = last)
              && not (slavo_Germanic(original))
              && string_at original (!current - 2) 2  [|"IE"|])
              && not (string_at original (!current - 4) 2  [|"ME";"MA"|]) then begin
            primary := !primary ^ "";
            secondary := !secondary ^ "R";
          end else begin
            primary := !primary ^ "R";
            secondary := !secondary ^ "R";
          end;
          if (String.sub original (!current + 1) 1) = "R" then begin
            incr current; incr current; end
          else
            incr current;
          raise Exit

        | "S" ->
          (*  special |s "island" "isle" "carlisle" "carlysle" *)
          if string_at original  (!current - 1) 3
                     [|"ISL";"YSL"|] then begin
            incr current;
            raise Exit
                     end;

          (*  special | "sugar-" *)
          if (!current = 0)
              && string_at original !current 5  [|"SUGAR"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "S";
            incr current;
            raise Exit
                        end;

          if string_at original  !current 2  [|"SH"|] then begin
            (*  germanic *)
            if string_at original  (!current + 1) 4
                       [|"HEIM";"HOEK";"HOLM";"HOLZ"|] then begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "S";
            end else begin
              primary := !primary ^ "X";
              secondary := !secondary ^ "X";
            end;
            incr current; incr current;
            raise Exit
                     end;

          (*  italian & armenian  *)
          if string_at original  !current 3  [|"SIO";"SIA"|]
              || string_at original !current 4  [|"SIAN"|] then begin
            if (not(slavo_Germanic(original))) then begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "X";
            end else begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "S";
            end;
            incr current; incr current; incr current;
            raise Exit
                        end;

          (*  german & anglicisations e.g. "smith" match "schmidt" "snider" match "schneider" *)
          (*  also -sz- in slavic language altho in hungarian it is pronounced "s" *)
          if ((!current = 0)
               && string_at original (!current + 1) 1 [|"M";"N";"L";"W"|])
              || string_at original (!current + 1) 1 [|"Z"|] then begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "X";
            if string_at original  (!current + 1) 1
                        [|"Z"|] then begin
              incr current; incr current; end
            else
              incr current;
            raise Exit
                        end;

          if string_at original  !current 2 [|"SC"|] then begin
            (*  Schlesinger's rule  *)
            if (String.sub original (!current + 2) 1) = "H" then begin
              (*  dutch origin e.g. "school" "schooner" *)
              if string_at original  (!current + 3) 2 [|"OO";"ER";"EN";"UY";"ED";"EM"|] then begin
                (*  "schermerhorn" "schenker"  *)
                if string_at original  (!current + 3) 2  [|"ER";"EN"|] then begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "SK";
                end else begin
                  primary := !primary ^ "SK";
                  secondary := !secondary ^ "SK";
                end;
                incr current; incr current; incr current;
                raise Exit
              end else begin
                if ((!current = 0)
                    && not (is_vowel original 3)
                    && (String.sub original (!current + 3) 1) <> "W") then begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "S";
                end else begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "X";
                end;
                incr current; incr current; incr current;
                raise Exit
              end;
            end else begin
              if string_at original  (!current + 2) 1 [|"I";"E";"Y"|] then begin
                primary := !primary ^ "S";
                secondary := !secondary ^ "S";
                incr current; incr current; incr current;
                raise Exit
              end;

            primary := !primary ^ "SK";
            secondary := !secondary ^ "SK";
            incr current; incr current; incr current;
            raise Exit

            end;
          end;

          (*  french e.g. "resnais" "artois" *)
          if (!current = last)
              && string_at original (!current - 2) 2 [|"AI";"OI"|] then begin
            primary := !primary ^ "";
            secondary := !secondary ^ "S";
          end else begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "S";
          end;

          if string_at original  (!current + 1) 1  [|"S";"Z"|] then begin
            incr current; incr current; end
          else
            incr current;
          raise Exit

        | "T" ->
          if string_at original  !current 4
                     [|"TION"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "X";
            incr current; incr current; incr current;
            raise Exit
                     end;

          if string_at original  !current 3
                     [|"TIA";"TCH"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "X";
            incr current; incr current; incr current;
            raise Exit
                     end;

          if string_at original  !current 2
                     [|"TH"|]
              || string_at original !current 3
                            [|"TTH"|] then begin
            (*  special | "thomas" "thames" or germanic *)
            if string_at original  (!current + 2) 2
                       [|"OM";"AM"|]
                || string_at original 0 4 [|"VAN ";"VON "|]
                || string_at original 0 3 [|"SCH"|] then begin
              primary := !primary ^ "T";
              secondary := !secondary ^ "T";
            end else begin
              primary := !primary ^ "0";
              secondary := !secondary ^ "T";
            end;
            incr current; incr current;
            raise Exit
                            end;

          if string_at original  (!current + 1) 1
                     [|"T";"D"|] then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "T";
          secondary := !secondary ^ "T";
          raise Exit

        | "V" ->
          if (String.sub original (!current + 1) 1) = "V" then begin
            incr current; incr current;end
          else
            incr current;
          primary := !primary ^ "F";
          secondary := !secondary ^ "F";
          raise Exit

        | "W" ->
          (*  can also be in middle of word *)
          if string_at original  !current 2 [|"WR"|] then begin
            primary := !primary ^ "R";
            secondary := !secondary ^ "R";
            incr current; incr current;
            raise Exit
          end;

          if (!current = 0)
              && (is_vowel original (!current + 1)
                  || string_at original !current 2
                            [|"WH"|]) then begin
            (*  Wasserman should match Vasserman  *)
            if (is_vowel original (!current + 1)) then begin
              primary := !primary ^ "A";
              secondary := !secondary ^ "F";
            end else begin
              (*  need Uomo to match Womo  *)
              primary := !primary ^ "A";
              secondary := !secondary ^ "A";
            end
              end;

          (*  Arnow should match Arnoff *)
          if (((!current = last)
                && is_vowel original (!current - 1))
              || string_at original (!current - 1) 5
                        [|"EWSKI";"EWSKY";"OWSKI";"OWSKY"|]
              || string_at original 0 3 [|"SCH"|]) then begin
            primary := !primary ^ "";
            secondary := !secondary ^ "F";
            incr current;
            raise Exit
          end;

          (*  polish e.g. "filipowicz" *)
          if string_at original  !current 4
                     [|"WICZ";"WITZ"|] then begin
            primary := !primary ^ "TS";
            secondary := !secondary ^ "FX";
            incr current; incr current; incr current; incr current;
            raise Exit
                     end;

          (*  else skip it *)
          incr current;
          raise Exit

        | "X" ->
          (*  french e.g. breaux  *)
          if (not ((!current = last)
                && (string_at original  (!current - 3) 3 [|"IAU"; "EAU"|]
                 || string_at original (!current - 2) 2 [|"AU"; "OU"|]))) then begin
            primary := !primary ^ "KS";
            secondary := !secondary ^ "KS";
          end;

          if string_at original  (!current + 1) 1  [|"C";"X"|] then begin
            incr current; incr current; end
          else
            incr current;
          raise Exit

        | "Z" ->
          (*  chinese pinyin e.g. "zhao"  *)
          if (String.sub original (!current + 1) 1) = "H" then begin
            primary := !primary ^ "J";
            secondary := !secondary ^ "J";
            incr current; incr current;
            raise Exit
          end else if string_at original  (!current + 1) 2  [|"ZO"; "ZI"; "ZA"|]
                    || (slavo_Germanic(original)
                        && ((!current > 0)
                            && String.sub original (!current - 1) 1 <> "T")) then begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "TS";
          end else begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "S";
          end;

          if (String.sub original (!current + 1) 1) = "Z" then begin
            incr current; incr current;end
          else
            incr current;
          raise Exit

        | _ ->
          incr current;

with Exit -> ())
    end; (*  end switch *)

    done; (*(*  end while*) *)
with Exit -> ());

    (!primary, !secondary)
