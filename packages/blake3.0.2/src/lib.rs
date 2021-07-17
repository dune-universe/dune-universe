#[ocaml::func]
pub fn blake3_hash(input: &[u8], output: &mut [u8]) -> () {
    let mut hasher = blake3::Hasher::new();
    hasher.update_with_join::<blake3::join::SerialJoin>(input);
    let mut output_reader = hasher.finalize_xof();
    output_reader.fill(output);
}

#[ocaml::func]
pub fn blake3_hash_multicore(input: &[u8], output: &mut [u8]) -> () {
    let mut hasher = blake3::Hasher::new();
    hasher.update_with_join::<blake3::join::RayonJoin>(input);
    let mut output_reader = hasher.finalize_xof();
    output_reader.fill(output);
}
