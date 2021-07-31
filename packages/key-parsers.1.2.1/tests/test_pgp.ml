open OUnit2
open Test_helpers
open Key_parsers.Pgp

exception File of string

let test_val ?(nth = 0) ~decode ~expected filename ctxt =
  let file = fixture filename in
  let res = decode file in
  let (public_packet : Packet.t) = Result.get_ok (List.nth res nth) in
  assert_equal ~ctxt public_packet.packet expected

module Rsa = struct
  let public_packet =
    let expected_public =
      let n =
        Z.of_string
          "0xfb4be4285f7bf636362c2a86b2680899c30d9d07851850b474e8db8d96743001917984e222e989751930c09c6c88dff8f17ecf8be3d4796bd18946ffae7be00a95055ad9d508225ce3dcec571a804fb15b34dcea1c52ec3720852a0c3c19a5772b7b2fa3c602209df84bd9756cacc040e0e9b5b7b6eb4fdee3ca6afeadbfea0b2d69064208467bcccc509affe01a8a49778b70150e8d9d289de5229f23b7a414ac7f3f7f196f71767c6b3ff3f2fd46505d8bed9a531a9c9ba0f5024e52173f62e025b84a3038cd39ffe22bf6a0d21bd5de60d5a51df8b364a5878404ae00a25e72e1cec494104e5adff481640bd17df825f191366895939ca588f287ec047348e0250a36e1c5873d7c95ddf8c945d89f344fe67230628d8b3d50124ee6306bba214103102509412e1c0e5df173b4834b061662e4678a73589639eedd256a36425edfea899882c4904c492d45027d1688417913f6c84758852be127695b0804a4537d9eee12ff9dba06b53eeb4456b79079c8e4dfb3ca01fdd3c3e248f5c79905"
      in
      let e = Z.of_string "0x10001" in
      Rsa.Public.{n; e}
    in
    let packet =
      Packet.Public_key.
        { version = 4
        ; public_key = Rsa expected_public
        ; algo = Algo.Public.Rsa_enc_sign
        ; creation_time = 1626248155l
        ; validity_period = None }
    in
    Packet.Body.Public_key packet

  let secret_packet =
    let expected_public2 =
      let public_key2 =
        let n =
          Z.of_string
            "0xc64c9e5d63cf62479608c22ba50cf2cbda2497e2cd92bdbb536ab4d55f1befaf262ae89a739698e538c688bce698aa4258bff01464f714ce98a2b1bacf92871f6e2b040990924f2c6634804072a990c6328ccd580e01f54b013aa0e08a388a78cd20ece56ca865079c26e77875c3b1f42d6bf015bb95da4606bf3fb05608ec17f823373dd73b75d77d1a34568797a690cbd21c6c78dd99d4f8cbea71e0327e8ca544619c244d1e9fa1a624209a7986b8663a1b1b0044899120d0daffddcf06df12159f8962c433a3b3ae7d84c5630ad290d97921b21a318a80d6a3b702d51cb2ba23958141a1702096cf891794f929e8396645f697e4f928a8105102f2ffed7b2ea920ffea992e74bfaeef564a6a06a7a61a0a379446367de345e9865205d095be574bb3bbc47721f71d4adaa79e2e4dcc781c94f90c64f32720a61b8dda1e6cf7093a2144276f772844242444cb06e7cf42a7745baaf4eb40b7975cc89aa32ff1d34fa8cadfc5a6aae59fff707c0d225e943ad479585af4bed5b71e78e183f3"
        in
        let e = Z.of_string "0x10001" in
        Rsa.Public.{n; e}
      in
      Packet.Public_key.
        { version = 4
        ; public_key = Rsa public_key2
        ; algo = Algo.Public.Rsa_enc_sign
        ; creation_time = 1626770303l
        ; validity_period = None }
    in

    let d =
      Z.of_string
        "0x12ad92d91f16a09e378e99a37cc76c733117f34a84ac86bc874decbc9059d448be229a8839ccfc97d2b2389cfa6774da0cfbff441c827d6e3c484ecf56cfe974576f2bd12ed1e92d386f47894a83984462ec8a46fa02cb3506679c51aa134b4fb79a9b55bcd75bb4cccb894ffa5fb57a5770ec4b4fc0fcaf4de163c15b4939dbc41ea50c69228ba6017d9f626b2128aa48d8809ee2ff9486cfec7d27bec362d2229aff4ab2cca4ceca6e18c70d65d6ffb57951a6486f2c3a044a47e683e10537814d46d26c70f7b927b20dcbb6b81663642f31b248a24a16c3dfaa97ce96c873b8dbe6781e82f450b95bf52ca2e2b7da1129c94ea308c583fee97140d738d1fabdc63515ed27f1d545c3237e44c122d3316b6a591daaf68deac8b3c694aa8658cb7b05b21cd94964e8a4b78c5fa2f7da22006132256984b0c2656f54028979cd81d2a1c1a6cae65fbd3eacc91948069d773e1652f9856df3dc425e6aff95d0d106b7daa53c4734e772550675508fbdbc6a6bcef367a37fd8e7f674ad5610a9b9"
    in
    let p =
      Z.of_string
        "0xdf789fe3ccebdf5cfe996e4f3550674b033684adf1297a8f77529c55413b2a0fab11317104e18200b94820d5bd2ab62867e965af7c9fab81033439371b22d2ed9046e2fb505d79eff7ee9a3c3521dd88e204ee897e853c5ea5e77e7764e79c0809d6f1e50a83e2e2729d6dca68d89c465d952ff0036d4ce8539159c3b8ac62938f318abfe04e686c56f294caa1801025fd06dba3c79e6484d33b724939b073912367d4afdc0def7751c176fc8dffad3c593efaf39f70eea9a15a4b8ad31ddee5"
    in
    let q =
      Z.of_string
        "0xe329fe9f5a911de7197985e89ce543e0b12720ae8bb596b6c70fb99ab03358d781b68f862e707006552f8cba1b2e97b6a711b21a62ff9c00c9eaa47432091894add02560f2bccdf90a40ec5456e9df8980804753ad7e1ddffb875e4605d48d6997503cbb0dd71bc458edd62251a079a3d3793f907d342d78a34790a0940c959d1f587840fa7d1efc66cdd8d893dc4844c0365707b31812c5cd509280c045dc8f85d9c7269875792e182301f918c4da7ae519ce67b0af953ff7a5478173d551f7"
    in
    let u =
      Z.of_string
        "0xaf96592890d66e7c762d11774d700877a3dbc1406a843547fe63f36715cdc11e0f0e8174577ca807dde961358ee44f59170a8ebd1eedc98f0c0b22125c111e6d72ebbaa2a2a477d492840ccbd0ca7f77915fa13b891da483e689671a7321176cb851605ef2970fbe4c8d3c33fb5f48c52ff9e0c7803e5bc6db000dcbf82b77772fc09a6405d1166b6cd8ac1f1b97d142d4730d31deb02f5bed6b12625ab3bc7d46150e2ee5ab432d02e2ccbec991cb4b71b8d43c7b9ef90807c909b302fd11da"
    in

    let private_key = Rsa.Private.{d; p; q; u} in
    let packet =
      Packet.Secret_key.
        { public_key = expected_public2
        ; s2k = None
        ; initial_vector = None
        ; private_key = Some (Rsa private_key)
        ; checksum = Some "e90c"
        ; hash = None }
    in
    Packet.Body.Secret_key packet

  let test_rsa_public =
    test_val ~decode ~expected:public_packet "rsa_public.pgp"

  let test_rsa_private =
    test_val ~decode ~expected:secret_packet "rsa_private.pgp"

  let suite = ["Public" >:: test_rsa_public; "Secret" >:: test_rsa_private]
end

module Dsa = struct
  let public_packet =
    let expected_public =
      let p =
        Z.of_string
          "0xd93edd8915add1c50daa9f232b17be4b47e8af10d92505dbcd095a332f583c2a64176faa557e896a56e662e8fe72859edce012ca0977dff484a01d969376f46853b4f03a3c23829a2b5b1012b84fbe733d6e384d80ac7406581ae0524f69cf8580d25efd6d976a8756cad44bcbd5c3e34a357f929a2121809ee252eb193af25e7b42d59fe2eb83ed342c5537300b486c1a90c549ac12b53993166c9e98cf04f0200814bd6601e5393ad63c45ccd97280a8c00a66f88f44432f3e9a4c9f76f5862fb0618f507238ee7f864b827c2ad23bee13963295ca3c5b070eefcd15c128989168166edc7b5398b5620373c6c7484193d03a9796dc2e9b51700b80a09e125f"
      in
      let q =
        Z.of_string
          "0xaaead1eefc59d06dbee40202852b95764f45121d2ec392cbd877b562635dd281"
      in
      let g =
        Z.of_string
          "0x3fa31581b723648fd0bf7d87fb5ec3b9bc764ded7ff4961acb05f281f9bc0886819f320e21d9a84375d2aca85eb2b35589f84cf647d7967f001de7267c2b5fd5c28cfbedc5686e50bf9c3153242371d67a2e9ea703ca22c03c6b2904cdc30bd582885dab36b53ea2401d7d834f58f560744f8f185363c25168e63692b08fc21c6e9c688b0fa467057e99721dbd19601632e68501fff7650dfba40a8cc7362f4a95f612db3126ac83df3e60f5d54625430396caf09cd86a3c11391a1bf32af967f2121a8f7a247c5c99b8965405ac6d29477fa1e0e00aa10741c5b1cebcaf0b3fdce32f745a929ff5bffc5c1a918b99dcea2ce38f1ac96a64e1a3ab1d918c3a5d"
      in
      let y =
        Z.of_string
          "0x5c428f6d55e36966e1aef9de9564c3e100b1979a3c7c19c01ce1aca55e4afa5c57c113bc48c53eee53146701c8e6ae6a445d93c9ec8cf568755eb1c56810ba7e06fd33f5b03e22595e8104aadadf5a58a883d3262ea579bdd6792c493bebaeb0bf3b5570396cc9a88927d51d5919dc50237cef2af387a2d2591596096a09fc8f0995a1ef9a5d33bafabd6432d668b965ae8c800801e6042c38e5800e8d2023bb24420d865d96fb9737f21e0c0838fb798ff6251ac4b8100b2a1abf27bc8b332c35accf37f0b65e692c4595dc23bda3fe06e6c322aa818c8b4963358ad8150369f622c15ac74d65e7235fef6be91401695f530277d8d6f5e2b0e4bb5bb0821455"
      in

      Dsa.Public.{p; q; g; y}
    in
    let packet =
      Packet.Public_key.
        { version = 4
        ; public_key = Dsa expected_public
        ; algo = Algo.Public.Dsa
        ; creation_time = 1626683055l
        ; validity_period = None }
    in
    Packet.Body.Public_key packet

  let secret_packet =
    let expected_public2 =
      let p =
        Z.of_string
          "0xd57323e2c1c98bf7311d8b17236834fe339e6c6ac383e7bf87b96a5acb166c373a694ad843f798fe5639e8264164d6f4b9234168d135844ea86536e502a047c6f61b6bbb7f0da0fb50e6c8bedb58b01d7c158f819bc19d149b81b2280ea5c075c7cb54eada34034bb88b5d1a435fa15ad8b06eca669e6fab40549507acfa419993218ff50f77714188dea705e044e022913cff09ee1a69447f64236da72f33402fcb770f31e0e07ffb80977b828039b4438acf0c7db467f856d6f39e235a783f5ea5134abc13677b99413b33c57eb33a044fe712c59268dd0b1e60db488ae1a7a78d16401011ccb2317214436c4ac3b97a36d91be232890119dfd6421db5038b"
      in
      let q =
        Z.of_string
          "0x93a788d267164f3c93845da64ef124aca0078eb0618c4a2f42a64e40ca701453"
      in
      let g =
        Z.of_string
          "0x1679af5c139e050c9925c46c37fb6795d73a7cb31f07b1106c141239a0f02ff6551a62b71faa2d26d5ab2d01775e57cd2c3aa53106e1c40b1bff0b6a064fa9a3092f1b7435116baeec272bbb9faa7d35b72a5635b9fdbe735c9c2c4edff4c0a56407418efde1bc0df5f512a5bfef9c49cf2f1137c5c335db0ff6fcd8e7c928b10e052ab11fca3e43f6d870d6405a20bd30bf117dc914338045626356cfe79c1cceed3d0cca1b2e5c91ea2ca3be530047764fca840c8dfa117b871a0a751e2d3ab5af0b84a682efb08d4fbd4e6fad60127313c6ba7517b56fdb29464b78bd0b799c0423a61dc4002c3ac8431a2880c0a0865fb901596142ff9c2fb4207dcb0986"
      in
      let y =
        Z.of_string
          "0xc8234a448a9bb87273c33a7b14eaa65b32418586443e4f1aab324d490e451bfe8024c569ce54cea5a6a797277d57ee40987d60bbc77510bbb593f08736289dd1535a793b6f3d35a80dd834911cf09f3a3ef2dea5a54f131afe65bb099ac9c7d165886fd36d54afcb8157a1da864f80eab2627fd20be76d47feee0e5e5cf7e316773b7f7db011fe1a955629293e5642c57f22447b89ddda33fb133d1fa89cca6e64f2bdb89d7c903edffa758588b29dcc5be49ed32c212af75926f0e6958527627fb363af10078530fbdc2cffbda945ca49b9691d3d2e860260afa2b1aec545eedc5d8ce014e66df32a138a836e61fab4813f2c059cce6b5d8635e0f0c10133d7"
      in

      let public_key = Dsa.Public.{p; q; g; y} in
      Packet.Public_key.
        { version = 4
        ; public_key = Dsa public_key
        ; algo = Algo.Public.Dsa
        ; creation_time = 1626774824l
        ; validity_period = None }
    in
    let x =
      Z.of_string
        "0x57ee36b7fae30f8324ba17dce280ffbdc43a13cd8a01ef2d8805fce70a0cf3d4"
    in
    let packet =
      Packet.Secret_key.
        { public_key = expected_public2
        ; s2k = None
        ; initial_vector = None
        ; private_key = Some (Dsa x)
        ; checksum = Some "1261"
        ; hash = None }
    in
    Packet.Body.Secret_key packet

  let test_dsa_public =
    test_val ~decode ~expected:public_packet "dsa_public.pgp"

  let test_dsa_secret =
    test_val ~decode ~expected:secret_packet "dsa_private.pgp"

  let suite = ["Public" >:: test_dsa_public; "Secret" >:: test_dsa_secret]
end

module Elgamal = struct
  let public_packet =
    let expected_public =
      let p =
        Z.of_string
          "0xd56ae8fb6b6bc0fc32e663a7a961607331bd7b41641a048516bfbbb7add148f1b3bffe7ce7544e29cd33c87cafd86b37e49e6d70273e53dcb0f7f3af63655a0c70f11dc4a7d397fbcd70a8666cffc8b6be3506960dcf091881e4faacf6c6530185431a4f96ed10a3e0ef35d9ccd9564c75e68685bff8fc09cd6322f2ea215f6da74aaf7b638027f954414614e0e4f90e7b972cdb607dc40dfa06f6c8a9bf3a04425f825ff5294dea2ab7d9922dc99bf986a617d39c7a1930de47d3afd94a722ee11c30733c4e98fc3095e857b3090a80e92cb19b61dd4def872d53f1787cee1a9806308ee1a319b686915e5ab98216e49fd7f26c12823c4c99c7277fcf02b74f"
      in
      let g = Z.of_string "0x5" in
      let y =
        Z.of_string
          "0x6751a59d5145c098ac9f997fcdc0fe139ba11050ad3ce40a049facd92395f6d44774e195c8fbad5f12f6365c7b07920f1af4bd10c106d89381d3529490ca437d44d42f71b1494dddb2238506cae5deef6e51def109e1c0d5f65ed6abafda5d59f5fff76603b2b9715da3c05a89c58ab5c542d5c17198ac799c6abd9cb690daf215f380a866b7d172e2ad486cd200f67afa6f7a05b74c8211a4f45bac7ca64beab87ff24ae8b748f09f609645370b2f5da4148544f15938df4dbcee581c0cd1df168e6b0341a7f0d0a2353ac58ee556b721c0bb7b92aea9dc4d70b36bc430f19b177ce80768e0aa8146e535b0d1d191848bb68039537ac9452cdfcefcffe75342"
      in
      Elgamal.Public.{p; g; y}
    in
    let packet =
      Packet.Public_key.
        { version = 4
        ; public_key = Elgamal expected_public
        ; algo = Algo.Public.Elgamal_enc_only
        ; creation_time = 1627053473l
        ; validity_period = None }
    in
    Packet.Body.Public_subkey packet

  let test_public =
    test_val ~nth:3 ~decode ~expected:public_packet "elgamal_public.pgp"

  let suite = ["Public" >:: test_public]
end

let id_packet =
  Packet.Body.Id Packet.Id.{name = "Clement "; email = "clement@test"}

module Test_errors = struct
  let test_rsa_tag0 ctxt =
    let file = fixture "rsa_tag0.pgp" in
    let res = decode file in
    let error = Result.get_error (List.hd res) in
    assert_equal ~ctxt error "Tag 0"

  let test_bad_algo ctxt =
    let file = fixture "bad_pub_algo.pgp" in
    let res = decode file in
    let error = Result.get_error (List.hd res) in
    assert_equal ~ctxt error "Unsupported algorithm: Unknown public algorithm"

  let test_bad_file ctxt =
    let file = fixture "bad_file.pgp" in
    let res = decode file in
    let error = Result.get_error (List.hd res) in
    assert_equal ~ctxt error "Bad header code"

  let suite =
    [ "Tag0" >:: test_rsa_tag0
    ; "Bad_algo" >:: test_bad_algo
    ; "Bad_file" >:: test_bad_file ]
end

let test_id = test_val ~nth:1 ~decode ~expected:id_packet "rsa_public.pgp"

let suite =
  [ "Rsa" >::: Rsa.suite
  ; "Dsa" >::: Dsa.suite
  ; "Id" >:: test_id
  ; "Errors" >::: Test_errors.suite
  ; "Elgamal" >::: Elgamal.suite ]
