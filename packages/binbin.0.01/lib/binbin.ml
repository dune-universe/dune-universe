type t =  string

let empty = ""

let zero_b = "0"

let one_b = "1"

let unsafe_s binstr = binstr

let unsafe_b binstr = binstr

let init n = String.make n '0'

let size binstr = String.length (unsafe_s binstr)

(** UTILITIES AND HELPERS **) 

let is_bit_on b = (b = one_b) 

let is_bit_off b = (b = zero_b)

let distance_to_byte b =
    let len = size b in
    if len < 8 then (8 - len)
    else ((-1) * len) mod 8

let ipow n x = (float_of_int n) ** (float_of_int x) |> int_of_float

let char_to_int c = if c = '0' then 0 else 1

let bin_to_int b = if b = zero_b then 0 else 1

let char_to_string c = String.make 1 c

let remove_str k s =
    let len = String.length s in
    if len > k then String.sub s k (len-k)
    else ""
 
let remove_bits i b =
    let len = (size b) in
    if len < i then
        invalid_arg "remove_bits: out-of-bound"
    else if i <= 0 then b
    else unsafe_b (String.sub (unsafe_s b) i (len-i))


let char_bit_to_binstr c =
    if c <> '0' && c <> '1' then
        invalid_arg "char_bit_to_binstr: invalid character bit"
    else unsafe_b (char_to_string c)

(** IMPLEMENTATION **)
let concat b1 b2 = unsafe_b ((unsafe_s b1) ^ (unsafe_s b2))

let take i b =
    if i = 0 then empty
    else if i > (size b) then
        invalid_arg "take: out-of-bound"
    else
        let s = unsafe_s b in
        char_bit_to_binstr s.[i-1]

let make i b =
    if (size b) = 0 || i = 0 then empty
    else begin
        let rec build pos i result model =
            if pos >= i then result
            else begin
                let model_pos = (pos mod (size model))+1 in
                let current = take model_pos model in
                build (pos+1) i (concat result current) model
            end
        in build 0 i empty b
    end 

let foldr f b acc =
    let rec traverse pos f b acc =
        if pos = 0 then acc
        else (traverse (pos-1) f b (f (take pos b) acc))
    in traverse (size b) f b acc


let foldl f acc b =
    let rec traverse pos f acc b =
        if pos > (size b) then acc
        else (traverse (pos+1) f (f acc (take pos b)) b)
    in traverse 1 f acc b

let map f b = foldr (fun x acc -> (concat (f x) acc)) b empty 

let mapi f b =
    if b = empty then empty
    else begin
        let pos = ref 0 in
        foldl (fun acc x ->
            let result = (f !pos x) in
            (pos := !pos + 1;
             (concat acc result))) empty b
    end

let msbit b =
    if (size b) = 0 then
        invalid_arg "msbit: empty binary"
    else (take 1 b)

let lsbit b =
    if (size b) = 0 then
        invalid_arg "lsbit: empty binary"
    else take (size b) b

let dmap f b1 b2 =
    if (size b1) <> (size b2) then
        invalid_arg "dmap: input binaries haven't been normalized";
    let rec traverse f acc b1 b2 =
        if (size b1) = 0 then acc
        else begin
            let msb1 = msbit b1 in
            let msb2 = msbit b2 in
            let result = f msb1 msb2 in
            let r1 = remove_bits 1 b1 in
            let r2 = remove_bits 1 b2 in
            traverse f (concat acc result) r1 r2
        end
    in traverse f empty b1 b2

let pad_left k b =
    let offset = (make k zero_b) in
    concat offset b

let byte_pad_left b =
    let padding = distance_to_byte b in
    pad_left padding b

let pad_right k b =
    let offset = (make k zero_b) in
    concat b offset

let byte_pad_right b =
    let padding = distance_to_byte b in
    pad_right padding b

let of_int n =
    let reduce_int i = if i = 0 then 0 else 1 in
    let rec convert pos binstr remainder =
        if remainder = 0 then binstr
        else begin
            let md = remainder mod (ipow 2 (pos+1)) in
            let current = (unsafe_b (string_of_int (reduce_int md))) in
            let updated = (concat current binstr) in
            convert (pos+1) updated (remainder-md)
        end
    in
    if n = 0 then (unsafe_b "0")
    else convert 0 empty n

let of_char c =
    let code = Char.code c in
    of_int code

let of_string s =
    let rec convert pos binstr remainder =
        if remainder = "" then binstr
        else begin
            let c = remainder.[0] in
            let c_binstr = byte_pad_left (of_char c) in
            let updated = (concat binstr c_binstr) in
            let r = remove_str 1 remainder in
            convert (pos+1) updated r
        end
    in (convert 0 empty s)

let to_int b =
    let f = fun x acc ->
        let coeff = bin_to_int x in
        let exp = (List.length acc) in
        let n = (ipow 2 exp) * coeff in
        (n :: acc)
    in
    let partial = foldr f b [] in
    List.fold_right (fun x acc -> x + acc) partial 0

let to_char b =
    let code = to_int b in
    (Char.chr code)

let to_ascii b =
    if b = empty then ""
    else begin
        let s = (unsafe_s (byte_pad_left b)) in
        let rec convert str binstr =
            let len_str = (String.length binstr) in
            if len_str = 0 then str
            else begin
                let byte = (String.sub binstr 0 8) in
                let remainder = remove_str 8 binstr in
                let binstr_byte = (to_int (unsafe_b byte)) in
                let c = (Char.chr binstr_byte) in
                let cs = (char_to_string c) in
                convert (str ^ cs) remainder
            end
        in convert "" s
    end

let normalize b1 b2 =
    let delta = (size b1) - (size b2) in
    if delta = 0 then (b1, b2)
    else if delta > 0 then (b1, (pad_left delta b2))
    else ((pad_left (abs delta) b1), b2)

let logical_operation op =
    fun bit1 bit2 ->
        let s1 = (unsafe_s bit1) in
        let s2 = (unsafe_s bit2) in
        let exec = op (char_to_int s1.[0]) (char_to_int s2.[0]) in
        (unsafe_b (string_of_int exec))

let b_xor b1 b2 =
    let padded_b1, padded_b2 = normalize b1 b2 in
    let xoring = logical_operation (lxor) in
    dmap xoring padded_b1 padded_b2 

let b_or b1 b2 =
    let padded_b1, padded_b2 = normalize b1 b2 in
    let oring = logical_operation (lor) in
    dmap oring padded_b1 padded_b2

let b_and b1 b2 =
    let padded_b1, padded_b2 = normalize b1 b2 in
    let anding = logical_operation (land) in
    dmap anding padded_b1 padded_b2

let b_not b =
    let upper_bound = (make (size b) (unsafe_b "1")) in
    b_xor b upper_bound

let flip b = b_not b

let flip_bit_at target b =
    if target > (size b) then
        raise (invalid_arg "flip_bit_at: out-of-bound");
    let flipper = fun index bit ->
        if target = (index+1) then (flip bit)
        else bit
    in mapi flipper b

let find_first_one b =
    let rec traverse pos b =
        if pos > (size b) then (pad_left (size b) (of_int 0))
        else if (take pos b) = (one_b) then (of_int pos)
        else traverse (pos+1) b
    in traverse 0 b

let count_leading_zeros b =
    let rec traverse count pos b =
        if pos > (size b) then pad_left (size b) (of_int count)
        else if (take pos b) = (one_b) then pad_left (size b) (of_int count)
        else traverse (count+1) (pos+1) b
    in traverse 0 1 b

let count_trailing_zeros b =
    let rec traverse count pos b =
        if pos > (size b) then pad_left (size b) (of_int count)
        else begin
            if (take pos b) = zero_b then traverse (count+1) (pos+1) b
            else traverse 0 (pos+1) b
        end
    in traverse 0 1 b

let irreducible b =
    let lz = (to_int (count_leading_zeros b)) in
    if lz = (size b) then b
    else remove_bits lz b

let reverse b = foldr (fun x acc -> (concat acc x)) b empty

let cardinality b =
    let f = fun bit acc ->
        if is_bit_on bit then (acc + 1)
        else acc
    in of_int (foldr (f) b 0)

let hamming_distance b1 b2 =
    let padded_b1, padded_b2 = normalize b1 b2 in
    let xored = b_xor padded_b1 padded_b2 in
    cardinality xored
