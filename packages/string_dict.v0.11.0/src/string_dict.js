//Provides: Base_string_dict_blocks_of_string
//Requires: caml_string_unsafe_get, caml_ml_string_length
function Base_string_dict_blocks_of_string(s) {
  var len = caml_ml_string_length(s);
  var len2 = ((len + 3) >> 2) << 2;
  var u8 = new joo_global_object.Uint8Array(len2);
  for(var i = 0; i < len; i++) u8[i] = caml_string_unsafe_get(s,i);
  var u32 = new joo_global_object.Uint32Array(u8.buffer,0, len2 >> 2);
  return u32;
}

//Provides: Base_string_dict_get_block
function Base_string_dict_get_block(blocks,offset) {
  return blocks[offset] >>> 0;
}

//Provides: Base_string_dict_num_blocks
function Base_string_dict_num_blocks(blocks) {
  return (blocks.length | 0);
}

//Provides: Base_string_dict_make_blocks
function Base_string_dict_make_blocks(blocks){
  var u32 = new joo_global_object.Uint32Array(blocks.length - 1);
  for(var i = 0; i < u32.length; i++){
    u32[i] = blocks[i+1];
  }
  return u32;
}

//Provides: Base_string_dict_find
//Requires: Base_string_dict_blocks_of_string
function Base_string_dict_find(t, key){
  key = Base_string_dict_blocks_of_string(key);
  var num_blocks = key.length;
  var input_blocks_idx = 0;
  for (; num_blocks; num_blocks--) {
    var input_block = key[input_blocks_idx++];
    var keys = t[2]; // Keys(t)
    var a = 0;
    var b = t[1] // num_children(t);
    /* It is likely that the first block is enough to distinguish all
       cases. So the remaining nodes will often form a chain. We
       optimize for this case. */
    if (b == 1) {
      if (input_block == keys[0])
        t = t[3][0+1]; // Child(t, 0)
      else
        return 0;
    } else {
      for (;;) {
        var c;
        var block;
        if (a >= b) return 0;
        c = (a + b) >> 1;
        block = keys[c];
        if (input_block < block)
          b = c;
        else if (input_block > block)
          a = c + 1;
        else {
          t = t[3][c+1]; //Child(t, c);
          break;
        }
      }
    }
  }
  return t[4] // Value(t);
}


