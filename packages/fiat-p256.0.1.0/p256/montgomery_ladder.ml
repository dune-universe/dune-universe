let scalar_mult d p =
  let r0 = ref (Point.at_infinity ()) in
  let r1 = ref p in
  for i = 255 downto 0 do
    if Scalar.bit_at d i then (
      r0 := Point.add !r0 !r1;
      r1 := Point.double !r1 )
    else (
      r1 := Point.add !r0 !r1;
      r0 := Point.double !r0 )
  done;
  !r0

let%expect_test "scalar mult" =
  let test ~scalar ~point =
    let scalar = Scalar.of_hex_exn scalar in
    let point = Point.of_hex_exn point in
    let res = scalar_mult scalar point in
    Format.printf "%a\n" Point.pp res
  in
  let point =
    `Hex
      "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5"
  in
  test
    ~scalar:
      (`Hex
        "0000000000000000000000000000000000000000000000000000000000000001")
    ~point;
  [%expect
    {| 046b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c2964fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5 |}];
  test
    ~scalar:
      (`Hex
        "0000000000000000000000000000000000000000000000000000000000000002")
    ~point;
  [%expect
    {| 047cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc4766997807775510db8ed040293d9ac69f7430dbba7dade63ce982299e04b79d227873d1 |}];
  test
    ~scalar:
      (`Hex
        "0000000000000000000000000000000000000000000000000000000000000004")
    ~point;
  [%expect
    {| 04e2534a3532d08fbba02dde659ee62bd0031fe2db785596ef509302446b030852e0f1575a4c633cc719dfee5fda862d764efc96c3f30ee0055c42c23f184ed8c6 |}];
  test
    ~scalar:
      (`Hex
        "0612465c89a023ab17855b0a6bcebfd3febb53aef84138647b5352e02c10c346")
    ~point:
      (`Hex
        "0462d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf");
  [%expect
    {| 0453020d908b0219328b658b525f26780e3ae12bcd952bb25a93bc0895e1714285b2ba871dd1652c3f467df15c6b70647efbcbbab5cbf7f55e6ff336f843d628a1 |}];
  test
    ~scalar:
      (`Hex
        "0a0d622a47e48f6bc1038ace438c6f528aa00ad2bd1da5f13ee46bf5f633d71a")
    ~point:
      (`Hex
        "043cbc1b31b43f17dc200dd70c2944c04c6cb1b082820c234a300b05b7763844c74fde0a4ef93887469793270eb2ff148287da9265b0334f9e2609aac16e8ad503");
  [%expect
    {| 047fffffffffffffffffffffffeecf2230ffffffffffffffffffffffffffffffff00000001c7c30643abed0af0a49fe352cb483ff9b97dccdf427c658e8793240d |}];
  test
    ~scalar:
      (`Hex
        "55d55f11bb8da1ea318bca7266f0376662441ea87270aa2077f1b770c4854a48")
    ~point:
      (`Hex
        "04000000000000000000000000000000000000000000000000000000000000000066485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4");
  [%expect
    {| 04cfe4077c8730b1c9384581d36bff5542bc417c9eff5c2afcb98cc8829b2ce8487764c65671a66a3ecf1ec63cf49b5c36119162ace73f8d8be270e27cdaf4677c |}]
