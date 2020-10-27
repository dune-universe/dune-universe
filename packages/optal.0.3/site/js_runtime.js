//Provides: caml_ml_output
function caml_ml_output (x, s, p, l) {
  var o = document.getElementById("output");
  o.appendChild (document.createTextNode(s.toString().slice(p,p+l)));
  return 0;
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
function caml_ml_output_char (x, c) {
    return caml_ml_output (x, String.fromCharCode (c), 0, 1);
}
