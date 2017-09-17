type 'a t = 'a OCSP.ref = {
  mutable contents: 'a;
}

let of_contents = OCSP.ref
let contents = OCSP.(!)
let assign = OCSP.(:=)

module O = struct
  let ref = OCSP.ref
  let (!) = OCSP.(!)
  let (:=) = OCSP.(:=)
end
