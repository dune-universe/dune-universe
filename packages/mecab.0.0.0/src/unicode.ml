(* MeCab --- A MeCab binding for OCaml

   Copyright (c) 2017 Akinori ABE

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open CamomileLibraryDefault.Camomile

module Char = struct
  (** {2 Character kinds} *)

  let between lower upper c = lower <= c && c <= upper

  (** {3 Top-level character ranges} *)

  (** [is_ascii c] checks [c] is an ASCII character: [[U+0000-U+007F]]. *)
  let is_ascii = between (UChar.of_int 0x0000) (UChar.of_int 0x007f)

  (** An alias of [is_ascii]. *)
  let is_basic_latin = is_ascii

  (** [is_latin_1 c] checks [c] is a Latin-1 character: [[U+00A0-U+00FF]]. *)
  let is_latin_1 = between (UChar.of_int 0x00a0) (UChar.of_int 0x00ff)

  (** [is_latin_a c] checks [c] is a Latin-A character: [[U+0100-U+017F]]. *)
  let is_latin1_a = between (UChar.of_int 0x0100) (UChar.of_int 0x017f)

  (** [is_latin_b c] checks [c] is a Latin-B character: [[U+0180-U+024F]]. *)
  let is_latin1_b = between (UChar.of_int 0x0180) (UChar.of_int 0x024f)

  (** [is_ipa_ext c] checks [c] is an IPA extension: [[U+0250-U+02AF]]. *)
  let is_ipa_ext = between (UChar.of_int 0x0250) (UChar.of_int 0x02af)

  (** [is_greek_coptic c] checks [c] is a Greek and Coptic letter:
      [[U+0370-U+03FF]]. *)
  let is_greek_coptic = between (UChar.of_int 0x0370) (UChar.of_int 0x03ff)

  (** [is_cyrillic c] checks [c] is a Cyrillic letter: [[U+0400-U+052F]]. *)
  let is_cyrillic = between (UChar.of_int 0x0400) (UChar.of_int 0x052f)

  (** [is_american c] checks [c] is an American letter: [[U+0530-U+058F]]. *)
  let is_american = between (UChar.of_int 0x0530) (UChar.of_int 0x058f)

  (** [is_hebrew c] checks [c] is a Hebrew letter: [[U+0590-U+05FF]]. *)
  let is_hebrew = between (UChar.of_int 0x0590) (UChar.of_int 0x05ff)

  (** [is_arabic c] checks [c] is an Arabic letter: [[U+0600-U+06FF]]. *)
  let is_arabic = between (UChar.of_int 0x0600) (UChar.of_int 0x06ff)

  (** [is_syriac c] checks [c] is a Syriac letter: [[U+0700-U+074F]]. *)
  let is_syriac = between (UChar.of_int 0x0700) (UChar.of_int 0x074f)

  (** [is_thaana c] checks [c] is a Thaana letter: [[U+0780-U+07BF]]. *)
  let is_thaana = between (UChar.of_int 0x0780) (UChar.of_int 0x07bf)

  (** [is_devanagari c] checks [c] is a Devanagari letter: [[U+0900-U+097F]]. *)
  let is_devanagari = between (UChar.of_int 0x0900) (UChar.of_int 0x097f)


  (** {3 ASCII characters} *)

  (** [is_ascii_digit c] checks [c] is an ASCII digit: [[0-9]]. *)
  let is_ascii_digit = between (UChar.of_int 0x0030) (UChar.of_int 0x0039)
  (** [is_ascii_upper c] checks [c] is an ASCII lower character: [[a-z]]. *)
  let is_ascii_upper = between (UChar.of_int 0x0041) (UChar.of_int 0x005a)
  (** [is_ascii_lower c] checks [c] is an ASCII upper character: [[A-Z]]. *)
  let is_ascii_lower = between (UChar.of_int 0x0061) (UChar.of_int 0x007a)
  (** [is_ascii_alpha c] checks [c] is an ASCII alphabet: [[a-zA-Z]]. *)
  let is_ascii_alpha c = is_ascii_upper c || is_ascii_lower c
  (** [is_ascii_alnum c] checks [c] is an ASCII digit or alphabet:
      [[a-zA-Z0-9]]. *)
  let is_ascii_alnum c = is_ascii_alpha c || is_ascii_digit c
  (** [is_ascii_word c] checks [c] is an ASCII word character: [[a-zA-Z0-9_]]. *)
  let is_ascii_word c = is_ascii_alnum c || c = UChar.of_int 0x5f
  (** [is_ascii_cntrl c] checks [c] is ASCII control character:
      [[\x00-\x1f\x7f]]. *)
  let is_ascii_cntrl c =
    between (UChar.of_int 0x00) (UChar.of_int 0x1f) c || c = UChar.of_int 0x7f
  (** [is_ascii_graph c] checks [c] is a visible character (anything except
      spaces and control characters): [\x21-\x7e]. *)
  let is_ascii_graph = between (UChar.of_int 0x0021) (UChar.of_int 0x007e)
  (** [is_ascii_print c] checks [c] is a visible character or a space:
      [\x20-\x7e]. *)
  let is_ascii_print = between (UChar.of_int 0x0020) (UChar.of_int 0x007e)
  (** [is_ascii_punct c] checks [c] is punctuation or symbol:
      [is_ascii_graph c && not (is_ascii_alnum c)]. *)
  let is_ascii_punct c = is_ascii_graph c && not (is_ascii_alnum c)
  (** [is_ascii_blank] checks [c] is a space or tab: [[ \t]]. *)
  let is_ascii_blank =
    let space = UChar.of_int 0x20 in
    let tab = UChar.of_int 0x09 in
    fun c -> c = space || c = tab
  (** [is_ascii_space c] checks [c] is whitespace: [[ \t\r\n\v\f]]. *)
  let is_ascii_space c = match UChar.int_of c with
    | 0x20 | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d -> true
    | _ -> false

  (** {3 Non-ASCII characters} *)

  (** [is_wide_ascii c] checks [c] is a full-width ASCII character. *)
  let is_wide_ascii = between (UChar.of_int 0xff00) (UChar.of_int 0xff5e)
  (** [is_wide_hira c] checks [c] is a Japanese hiragana. *)
  let is_wide_hira = between (UChar.of_int 0x3040) (UChar.of_int 0x309f)
  (** [is_wide_kana c] checks [c] is a Japanese full-width katakana. *)
  let is_wide_kana = between (UChar.of_int 0x30a0) (UChar.of_int 0x30ff)
  (** [is_wide_kana c] checks [c] is a Japanese half-width katakana. *)
  let is_half_kana = between (UChar.of_int 0xff66) (UChar.of_int 0xff9f)
  (** [is_kana c] is [is_half_kana c || is_wide_kana c]. *)
  let is_kana c = is_half_kana c || is_wide_kana c
  (** [is_kanji c] checks [c] is a Chinese or Japanese kanji. *)
  let is_kanji = between (UChar.of_int 0x4e00) (UChar.of_int 0x9fff)

  (** [is_blank c] checks [c] is
      - [U+0009] horizontal tab,
      - [U+0020] space,
      - [U+00a0] non-breaking space,
      - [U+2002-U+200B] various width space,
      - [U+3000] ideographic space, or
      - [U+ffef] zero width non-breaking space. *)
  let is_blank c = match UChar.int_of c with
    | 0x0009 | 0x0020 | 0x00a0 | 0x3000 | 0xffef -> true
    | _ -> between (UChar.of_int 0x2002) (UChar.of_int 0x200b) c
end

module type StringIntf =
sig
  type t
  type index = int

  val init : int -> (int -> UChar.t) -> t
  val get : t -> int -> UChar.t
  val first : t -> index
  val last : t -> index
  val look : t -> index -> UChar.t
  val next : t -> index -> index
  val prev : t -> index -> index
  val out_of_range : t -> index -> bool

  val length : t -> int

  module Buf : sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end
end

module Make(S : StringIntf) =
struct
  (** {2 Construction} *)

  let make n c = S.init n (fun _ -> c)

  let init ~f n = S.init n f

  let of_ascii_string s =
    init (String.length s) ~f:(fun i -> UChar.of_char s.[i])

  let to_ascii_string s =
    String.init (S.length s) (fun i -> UChar.char_of (S.get s i))

  (** {2 Iteration} *)

  let foldi ~f ~init s =
    let rec aux acc i =
      if S.out_of_range s i then acc
      else aux (f i acc (S.look s i)) (S.next s i)
    in
    aux init (S.first s)

  let fold ~f ~init s = foldi ~f:(fun _ acc c -> f acc c) ~init s

  let iteri ~f s =
    let rec aux i =
      if not (S.out_of_range s i) then ( f i (S.look s i) ; aux (S.next s i) )
    in
    aux (S.first s)

  let iter ~f = iteri ~f:(fun _ -> f)

  let mapi ~f s =
    let b = S.Buf.create (S.length s) in
    iteri ~f:(fun i c -> S.Buf.add_char b (f i c)) s ;
    S.Buf.contents b

  let map ~f = mapi ~f:(fun _ -> f)

  let filter_mapi ~f s =
    let b = S.Buf.create (S.length s) in
    iteri s
      ~f:(fun i c -> match f i c with
          | Some c' -> S.Buf.add_char b c'
          | None -> ()) ;
    S.Buf.contents b

  let filter_map ~f = filter_mapi ~f:(fun _ -> f)

  let concat_map ~f s =
    let b = S.Buf.create (S.length s) in
    iter s ~f:(fun c -> S.Buf.add_string b (f c)) ;
    S.Buf.contents b

  (** {2 Conversion} *)

  let to_list_map ~f s =
    let rec aux chrs i =
      if S.out_of_range s i then chrs
      else aux ((f (S.look s i)) :: chrs) (S.prev s i)
    in
    aux [] (S.last s)

  let to_list = to_list_map ~f:(fun c -> c)

  let of_list_map ~f chrs =
    let b = S.Buf.create 16 in
    List.iter (fun c -> S.Buf.add_char b (f c)) chrs ;
    S.Buf.contents b

  let of_list = of_list_map ~f:(fun c -> c)

  let trim ?(drop = Char.is_blank) s =
    let b = S.Buf.create (S.length s) in
    let rec add i j =
      if i > j then S.Buf.contents b
      else ( S.Buf.add_char b (S.look s i) ; add (S.next s i) j ) in
    let rec skip_tail i j =
      if S.out_of_range s j then S.Buf.contents b
      else if drop (S.look s j) then skip_tail i (S.prev s j)
      else add i j in
    let rec skip_head i =
      if S.out_of_range s i then S.Buf.contents b
      else if drop (S.look s i) then skip_head (S.next s i)
      else skip_tail i (S.last s) in
    skip_head (S.first s)

  (** [uppercase_ascii str] replaces all occurrence of ASCII lowercase alphabets
      in string [str] into uppercase alphabets. *)
  let uppercase_ascii str =
    let shift c = UChar.of_int (UChar.int_of c - 0x61 + 0x41) in
    map ~f:(fun c -> if Char.is_ascii_lower c then shift c else c) str

  (** [lowercase_ascii str] replaces all occurrence of ASCII uppercase alphabets
      in string [str] with lowercase alphabets. *)
  let lowercase_ascii str =
    let shift c = UChar.of_int (UChar.int_of c + 0x61 - 0x41) in
    map ~f:(fun c -> if Char.is_ascii_upper c then shift c else c) str

  (** [latinize_number_form str] replaces all occurrence of non-ASCII number
      forms [[U+2150-U+218B]] in string [str] with ASCII characters. *)
  let latinize_number_form =
    let tbl = Array.map of_ascii_string [|
        "1/7"; "1/9"; "1/10"; "1/3"; "2/3"; "1/5"; "2/5"; "3/5"; "4/5";
        "1/6"; "5/6"; "1/8"; "3/8"; "5/8"; "7/8"; "1/";
        "I"; "II"; "III"; "IV"; "V"; "VI"; "VII"; "VIII"; "IX";
        "X"; "XI"; "XII"; "L"; "C"; "D"; "M";
        "i"; "ii"; "iii"; "iv"; "v"; "vi"; "vii"; "viii"; "ix";
        "x"; "xi"; "xii"; "l"; "c"; "d"; "m";
      |] in
    concat_map ~f:(fun c -> match UChar.int_of c with
        | code when 0x2150 <= code && code <= 0x217f -> tbl.(code - 0x2150)
        | 0x2189 -> of_ascii_string "0/3" (* ↉ *)
        | 0x218a -> of_ascii_string "2" (* ↊ *)
        | 0x218b -> of_ascii_string "3" (* ↋ *)
        | _ -> make 1 c)

  (** [latinize_enclosed_alpha str] replaces all occurrence of non-ASCII enclosed
      alphabets [[U+2460-U+24FF]] in string [str] with ASCII characters. *)
  let latinize_enclosed_alpha =
    let tbl = Array.map of_ascii_string [|
        "(1)"; "(2)"; "(3)"; "(4)"; "(5)"; "(6)"; "(7)"; "(8)"; "(9)"; "(10)";
        "(11)"; "(12)"; "(13)"; "(14)"; "(15)"; "(16)"; "(17)"; "(18)"; "(19)";
        "(20)"; "(1)"; "(2)"; "(3)"; "(4)"; "(5)"; "(6)"; "(7)"; "(8)"; "(9)";
        "(10)"; "(11)"; "(12)"; "(13)"; "(14)"; "(15)"; "(16)"; "(17)"; "(18)";
        "(19)"; "(20)"; "1."; "2."; "3."; "4."; "5."; "6."; "7."; "8."; "9.";
        "10."; "11."; "12."; "13."; "14."; "15."; "16."; "17."; "18."; "19.";
        "20."; "(a)"; "(b)"; "(c)"; "(d)"; "(e)"; "(f)"; "(g)"; "(h)"; "(i)";
        "(j)"; "(k)"; "(l)"; "(m)"; "(n)"; "(o)"; "(p)"; "(q)"; "(r)"; "(s)";
        "(t)"; "(u)"; "(v)"; "(w)"; "(x)"; "(y)"; "(z)"; "(A)"; "(B)"; "(C)";
        "(D)"; "(E)"; "(F)"; "(G)"; "(H)"; "(I)"; "(J)"; "(K)"; "(L)"; "(M)";
        "(N)"; "(O)"; "(P)"; "(Q)"; "(R)"; "(S)"; "(T)"; "(U)"; "(V)"; "(W)";
        "(X)"; "(Y)"; "(Z)"; "(a)"; "(b)"; "(c)"; "(d)"; "(e)"; "(f)"; "(g)";
        "(h)"; "(i)"; "(j)"; "(k)"; "(l)"; "(m)"; "(n)"; "(o)"; "(p)"; "(q)";
        "(r)"; "(s)"; "(t)"; "(u)"; "(v)"; "(w)"; "(x)"; "(y)"; "(z)"; "(0)";
        "(11)"; "(12)"; "(13)"; "(14)"; "(15)"; "(16)"; "(17)"; "(18)"; "(19)";
        "(20)"; "(1)"; "(2)"; "(3)"; "(4)"; "(5)"; "(6)"; "(7)"; "(8)"; "(9)";
        "(10)"; "(0)";
      |] in
    concat_map ~f:(fun c -> match UChar.int_of c with
        | code when 0x2460 <= code && code <= 0x24ff -> tbl.(code - 0x2460)
        | _ -> make 1 c)

  (** [latinize_blank str] replaces all occurrence of blank-like characters
      (that satisfy [Char.is_blank]) with ASCII spaces [U+0020]. *)
  let latinize_blank =
    map ~f:(fun c -> if Char.is_blank c then UChar.of_int 0x20 else c)

  (** [halve_ascii str] replaces all occurrence of full-width ASCII
      characters (that satisfy [Char.is_wide_ascii]) with corresponding ASCII
      characters. *)
  let halve_ascii =
    map ~f:(fun c -> match UChar.int_of c with
        | 0xffe5 -> UChar.of_int 0x005c (* full-width YEN to half-width backslash *)
        | code when Char.is_wide_ascii c -> UChar.of_int (code - 0xff00 + 0x20)
        | _ -> c)

  (** [widen_kana str] converts all occurrence of half-width katakanas in
      string [str] into full-width katakanas. *)
  let widen_kana str =
    let h_a = 0xff71 and z_a = 0x30a2
    and h_ka = 0xff76 and z_ka = 0x30ab and z_ga = 0x30ac
    and h_tu = 0xff82 and h_to = 0xff84 and z_tu = 0x30c4 and z_du = 0x30c5
    and h_na = 0xff85 and z_na = 0x30ca
    and h_ha = 0xff8a and h_ho = 0xff8e and z_ha = 0x30cf and z_ba = 0x30d0 and z_pa = 0x30d1
    and h_ma = 0xff8f and z_ma = 0x30de
    and h_ya = 0xff94 and z_ya = 0x30e4
    and h_ra = 0xff97 and h_ro = 0xff9b and z_ra = 0x30e9
    and h_xa = 0xff67 and h_xo = 0xff6b and z_xa = 0x30a1 in
    let b = S.Buf.create (S.length str) in
    let put c = S.Buf.add_char b (UChar.of_int c) in
    let rec aux = function
      (* Convert half-width katakana to full-width katakana *)
      | 0xff9c :: tl -> put 0x30ef ; aux tl (* ﾜ *)
      | 0xff66 :: tl -> put 0x30f2 ; aux tl (* ｦ *)
      | 0xff9d :: tl -> put 0x30f3 ; aux tl (* ﾝ *)
      | 0xff6c :: tl -> put 0x30e3 ; aux tl (* ｬ *)
      | 0xff6d :: tl -> put 0x30e5 ; aux tl (* ｭ *)
      | 0xff6e :: tl -> put 0x30e7 ; aux tl (* ｮ *)
      | 0xff6f :: tl -> put 0x30c3 ; aux tl (* ｯ *)
      | 0xff61 :: tl -> put 0x3002 ; aux tl (* ｡ *)
      | 0xff62 :: tl -> put 0x300c ; aux tl (* ｢ *)
      | 0xff63 :: tl -> put 0x300d ; aux tl (* ｣ *)
      | 0xff64 :: tl -> put 0x3001 ; aux tl (* ､ *)
      | 0xff65 :: tl -> put 0x30fb ; aux tl (* ･ *)
      | c :: 0xff9f :: tl when h_ha <= c && c <= h_ho -> put ((c - h_ha) * 3 + z_pa) ; aux tl (* ﾊﾟ-ﾎﾟ *)
      | c :: 0xff9e :: tl when h_ha <= c && c <= h_ho -> put ((c - h_ha) * 3 + z_ba) ; aux tl (* ﾊﾞ-ﾎﾞ *)
      | c :: 0xff9e :: tl when h_ka <= c && c <  h_tu -> put ((c - h_ka) * 2 + z_ga) ; aux tl (* ｶﾞ-ﾁﾞ *)
      | c :: 0xff9e :: tl when h_tu <= c && c <= h_to -> put ((c - h_tu) * 2 + z_du) ; aux tl (* ﾂﾞ-ﾄﾞ *)
      | c :: tl when h_xa <= c && c <= h_xo -> put ((c - h_xa) * 2 + z_xa) ; aux tl (* ｧ-ｫ *)
      | c :: tl when h_a  <= c && c <  h_ka -> put ((c - h_a) * 2 + z_a) ; aux tl (* ｱ-ｵ *)
      | c :: tl when h_ka <= c && c <  h_tu -> put ((c - h_ka) * 2 + z_ka) ; aux tl (* ｶ-ﾁ *)
      | c :: tl when h_tu <= c && c <= h_to -> put ((c - h_tu) * 2 + z_tu) ; aux tl (* ﾂ-ﾄ *)
      | c :: tl when h_na <= c && c <  h_ha -> put (c - h_na + z_na) ; aux tl (* ﾅ-ﾉ *)
      | c :: tl when h_ha <= c && c <= h_ho -> put ((c - h_ha) * 3 + z_ha) ; aux tl (* ﾊ-ﾎ *)
      | c :: tl when h_ma <= c && c <  h_ya -> put ((c - h_ma + z_ma)) ; aux tl (* ﾏ-ﾓ *)
      | c :: tl when h_ya <= c && c <  h_ra -> put ((c - h_ya) * 2 + z_ya) ; aux tl (* ﾔ-ﾖ *)
      | c :: tl when h_ra <= c && c <= h_ro -> put ((c - h_ra + z_ra)) ; aux tl (* ﾗ-ﾛ *)
      (* Others *)
      | c :: tl -> put c ; aux tl
      | [] -> S.Buf.contents b
    in
    aux (to_list_map ~f:UChar.int_of str)

  (** Neologd-style normalization for Japanese texts.

     @see <https://github.com/neologd/mecab-ipadic-neologd>
     @see <https://github.com/neologd/mecab-ipadic-neologd/wiki/Regexp.ja> *)
  let neologd_normalize str =
    let rev_remove_verbosity =
      let rec aux acc = function
        (* Remove contiguous characters *)
        | 0x0020 :: (0x0020 :: _ as tl) -> aux acc tl (* Spaces *)
        | 0x30fc :: (0x30fc :: _ as tl) -> aux acc tl (* Prolonged Sound Marks *)
        (* Others *)
        | c :: tl -> aux (c :: acc) tl
        | [] -> acc
      in
      aux []
    in
    let rev_remove_padding =
      let is_digit code = Char.is_ascii_digit (UChar.of_int code) in
      let is_alnum code = Char.is_ascii_alnum (UChar.of_int code) in
      let is_cjk_punct code =
        let c = UChar.of_int code in
        Char.is_kanji c || Char.is_wide_hira c || Char.is_wide_kana c ||
        Char.is_ascii_punct c || Char.between 0xff00 0xffef code in
      let rec aux acc = function
        (* Remove spaces between kanji, punctuation, or alnum. *)
        | c1 :: 0x20 :: (c2 :: _ as tl) ->
          if is_cjk_punct c1 && is_cjk_punct c2 then aux (c1 :: acc) tl
          else if is_cjk_punct c1 && is_alnum c2 then aux (c1 :: acc) tl
          else if is_alnum c1 && is_cjk_punct c2 then aux (c1 :: acc) tl
          else aux (0x20 :: c1 :: acc) tl
        (* Remove commas in numbers, e.g., "10,000" |-> "10000" *)
        | c1 :: 0x2c :: c2 :: tl when is_digit c1 && is_digit c2 ->
          aux (c2 :: c1 :: acc) tl
        (* Others *)
        | c :: tl -> aux (c :: acc) tl
        | [] -> acc
      in
      aux []
    in
    latinize_blank str
    |> halve_ascii
    |> widen_kana
    |> to_list_map ~f:UChar.int_of
    |> List.fold_left (fun acc -> function
        (* Convert equals to full-width equals (due to human names) *)
        | 0x003d | 0xff1d -> 0xff1d :: acc
        (* Convert hyphen-like letters to hyphen minus *)
        | 0x02d7 (* Modifier Letter Minus Sign *)
        | 0x058a (* Armenian Hyphen *)
        | 0x2010 (* Hyphen *)
        | 0x2011 (* Non breaking Hyphen *)
        | 0x2012 (* Figure Dash *)
        | 0x2013 (* EN Dash *)
        | 0x2043 (* Hyphen bullet *)
        | 0x207b (* Superscript Minus *)
        | 0x208b (* Subscript Minus *)
        | 0x2212 (* Negative Sign *) -> 0x002d :: acc
        (* Convert prolonged-sound-like signs to full-width prolonged sound sign *)
        | 0x2014 (* EM Dash *)
        | 0x2015 (* Horizontal Line *)
        | 0x2500 (* Box Drawings Light Horizontal *)
        | 0x2501 (* Box Drawings Heavy Horizontal *)
        | 0xfe63 (* Small Hyphen Minus *)
        | 0xff0d (* Full-width Hyphen Minus *)
        | 0xff70 (* Half-width Prolonged Sound Sign *) -> 0x30fc :: acc
        (* Convert full-width quotes to half-width quotes *)
        | 0x2018 (* Left Single Quotation *)
        | 0x2019 (* Right Single Quotation *)
        | 0x201a (* Single Low-9 Quotation *)
        | 0x201b (* Single High-Reversed-9 Quotation *)
        | 0x2032 (* Prime *)
        | 0x2035 (* Reversed Prime *) -> 0x0027 :: acc
        | 0x201c (* Left Double Quotation *)
        | 0x201d (* Right Double Quotation *)
        | 0x201e (* Double Low-9 Quotation *)
        | 0x201f (* Double High-Reversed-9 Quotation *)
        | 0x2033 (* Double Prime *)
        | 0x2036 (* Reversed Double Prime *) -> 0x0022 :: acc
        (* Remove tilde-like characters *)
        | 0x007e (* Tilde *)
        | 0x02dc (* Small Tilde *)
        | 0x223c (* Tilde Operator *)
        | 0x223d (* Reversed Tilde *)
        | 0x223e (* Inverted Lazy S *)
        | 0x2053 (* Swung Dash *)
        | 0x301c (* Wave Dash *)
        | 0x3030 (* Wavy Dash *) -> acc
        | code -> code :: acc) []
    |> rev_remove_verbosity
    |> rev_remove_padding
    |> List.rev
    |> of_list_map ~f:UChar.of_int
    |> trim
end

module UTF8 = Make(CamomileLibraryDefault.Camomile.UTF8)
