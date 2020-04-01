(** A ChaCha testsuite, with test vectors taken from
    {{:https://tools.ietf.org/html/draft-strombergson-chacha-test-vectors-01}
    Test Vectors for the Stream Cipher ChaCha} *)

let test_chacha ~hash ~key ~nonce ~output0 ~output1 =
  let open Cstruct in
  let open Alcotest in
  let key = of_hex key
  and nonce = of_hex nonce
  and input = Cstruct.create 128
  and output0 = output0 |> of_hex |> to_string
  and output1 = output1 |> of_hex |> to_string in
  fun () ->
    let stream =
      Chacha.create ~hash key nonce
      |> Chacha.encrypt input
      |> to_string in
    check int "Chacha test output length" (Cstruct.len input) (String.length stream);
    check string "Chacha test block 0 value" (String.sub stream 0 64) output0;
    check string "Chacha test block 1 value" (String.sub stream 64 64) output1
  
let test_chacha20 ~key ~nonce ~output0 ~output1 =
  test_chacha ~hash:Chacha_core.chacha20 ~key ~nonce ~output0 ~output1

let test_chacha12 ~key ~nonce ~output0 ~output1 =
  test_chacha ~hash:Chacha_core.chacha12 ~key ~nonce ~output0 ~output1

let test_chacha8 ~key ~nonce ~output0 ~output1 =
  test_chacha ~hash:Chacha_core.chacha8 ~key ~nonce ~output0 ~output1

let chacha20_256_1_test =
  test_chacha20
    ~key:(String.make 64 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("76b8e0ada0f13d90405d6ae55386bd28" ^
              "bdd219b8a08ded1aa836efcc8b770dc7" ^
              "da41597c5157488d7724e03fb8d84a37" ^
              "6a43b8f41518a11cc387b669b2ee6586")
    ~output1:("9f07e7be5551387a98ba977c732d080d" ^
              "cb0f29a048e3656912c6533e32ee7aed" ^
              "29b721769ce64e43d57133b074d839d5" ^
              "31ed1f28510afb45ace10a1f4b794d6f")

let chacha20_256_2_test =
  test_chacha20
    ~key:("01" ^ String.make 62 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("c5d30a7ce1ec119378c84f487d775a85" ^
              "42f13ece238a9455e8229e888de85bbd" ^
              "29eb63d0a17a5b999b52da22be4023eb" ^
              "07620a54f6fa6ad8737b71eb0464dac0")
    ~output1:("10f656e6d1fd55053e50c4875c9930a3" ^
              "3f6d0263bd14dfd6ab8c70521c19338b" ^
              "2308b95cf8d0bb7d202d2102780ea352" ^
              "8f1cb48560f76b20f382b942500fceac")

let chacha20_256_3_test =
  test_chacha20
    ~key:(String.make 64 '0')
    ~nonce:("01" ^ String.make 14 '0')
    ~output0:("ef3fdfd6c61578fbf5cf35bd3dd33b80" ^
              "09631634d21e42ac33960bd138e50d32" ^
              "111e4caf237ee53ca8ad6426194a8854" ^
              "5ddc497a0b466e7d6bbdb0041b2f586b")
    ~output1:("5305e5e44aff19b235936144675efbe4" ^
              "409eb7e8e5f1430f5f5836aeb49bb532" ^
              "8b017c4b9dc11f8a03863fa803dc71d5" ^
              "726b2b6b31aa32708afe5af1d6b69058")

let chacha20_256_4_test =
  test_chacha20
    ~key:(String.make 64 'f')
    ~nonce:(String.make 16 'f')
    ~output0:("d9bf3f6bce6ed0b54254557767fb5744" ^
              "3dd4778911b606055c39cc25e674b836" ^
              "3feabc57fde54f790c52c8ae43240b79" ^
              "d49042b777bfd6cb80e931270b7f50eb")
    ~output1:("5bac2acd86a836c5dc98c116c1217ec3" ^
              "1d3a63a9451319f097f3b4d6dab07787" ^
              "19477d24d24b403a12241d7cca064f79" ^
              "0f1d51ccaff6b1667d4bbca1958c4306")

let chacha20_256_5_test =
  test_chacha20
    ~key:(String.make 64 '5')
    ~nonce:(String.make 16 '5')
    ~output0:("bea9411aa453c5434a5ae8c92862f564" ^
              "396855a9ea6e22d6d3b50ae1b3663311" ^
              "a4a3606c671d605ce16c3aece8e61ea1" ^
              "45c59775017bee2fa6f88afc758069f7")
    ~output1:("e0b8f676e644216f4d2a3422d7fa36c6" ^
              "c4931aca950e9da42788e6d0b6d1cd83" ^
              "8ef652e97b145b14871eae6c6804c700" ^
              "4db5ac2fce4c68c726d004b10fcaba86")

let chacha20_256_6_test =
  test_chacha20
    ~key:(String.make 64 'a')
    ~nonce:(String.make 16 'a')
    ~output0:("9aa2a9f656efde5aa7591c5fed4b35ae" ^
              "a2895dec7cb4543b9e9f21f5e7bcbcf3" ^
              "c43c748a970888f8248393a09d43e0b7" ^
              "e164bc4d0b0fb240a2d72115c4808906")
    ~output1:("72184489440545d021d97ef6b693dfe5" ^
              "b2c132d47e6f041c9063651f96b623e6" ^
              "2a11999a23b6f7c461b2153026ad5e86" ^
              "6a2e597ed07b8401dec63a0934c6b2a9")

let chacha20_256_7_test =
  test_chacha20
    ~key:"00112233445566778899aabbccddeeffffeeddccbbaa99887766554433221100"
    ~nonce:"0f1e2d3c4b5a6978"
    ~output0:("9fadf409c00811d00431d67efbd88fba" ^
              "59218d5d6708b1d685863fabbb0e961e" ^
              "ea480fd6fb532bfd494b215101505742" ^
              "3ab60a63fe4f55f7a212e2167ccab931")
    ~output1:("fbfd29cf7bc1d279eddf25dd316bb884" ^
              "3d6edee0bd1ef121d12fa17cbc2c574c" ^
              "ccab5e275167b08bd686f8a09df87ec3" ^
              "ffb35361b94ebfa13fec0e4889d18da5")

let chacha20_256_8_test =
  test_chacha20
    ~key:"c46ec1b18ce8a878725a37e780dfb7351f68ed2e194c79fbc6aebee1a667975d"
    ~nonce:"1ada31d5cf688221"
    ~output0:("f63a89b75c2271f9368816542ba52f06" ^
              "ed49241792302b00b5e8f80ae9a473af" ^
              "c25b218f519af0fdd406362e8d69de7f" ^
              "54c604a6e00f353f110f771bdca8ab92")
    ~output1:("e5fbc34e60a1d9a9db17345b0a402736" ^
              "853bf910b060bdf1f897b6290f01d138" ^
              "ae2c4c90225ba9ea14d518f55929dea0" ^
              "98ca7a6ccfe61227053c84e49a4a3332")

let chacha20_256_bit_tests = [
  "TC1: All zero key and IV.", `Quick, chacha20_256_1_test;
  "TC2: Single bit in key set. All zero IV.", `Quick, chacha20_256_2_test;
  "TC3: Single bit in IV set. All zero key.", `Quick, chacha20_256_3_test;
  "TC4: All bits in key and IV are set.", `Quick, chacha20_256_4_test;
  "TC5: Every even bit set in key and IV.", `Quick, chacha20_256_5_test;
  "TC6: Every odd bit set in key and IV.", `Quick, chacha20_256_6_test;
  "TC7: Sequence patterns in key and IV.", `Quick, chacha20_256_7_test;
  "TC8: key: 'All your base are belong to us!, IV: 'IETF2013'", `Quick, chacha20_256_8_test;
]

let chacha20_128_1_test =
  test_chacha20
    ~key:(String.make 32 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("89670952608364fd00b2f90936f031c8" ^
              "e756e15dba04b8493d00429259b20f46" ^
              "cc04f111246b6c2ce066be3bfb32d9aa" ^
              "0fddfbc12123d4b9e44f34dca05a103f")
    ~output1:("6cd135c2878c832b5896b134f6142a9d" ^
              "4d8d0d8f1026d20a0a81512cbce6e975" ^
              "8a7143d021978022a384141a80cea306" ^
              "2f41f67a752e66ad3411984c787e30ad")
    
let chacha20_128_2_test =
  test_chacha20
    ~key:("01" ^ String.make 30 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("ae56060d04f5b597897ff2af1388dbce" ^
              "ff5a2a4920335dc17a3cb1b1b10fbe70" ^
              "ece8f4864d8c7cdf0076453a8291c7db" ^
              "eb3aa9c9d10e8ca36be4449376ed7c42")
    ~output1:("fc3d471c34a36fbbf616bc0a0e7c5230" ^
              "30d944f43ec3e78dd6a12466547cb4f7" ^
              "b3cebd0a5005e762e562d1375b7ac445" ^
              "93a991b85d1a60fba2035dfaa2a642d5")
    
let chacha20_128_3_test =
  test_chacha20
    ~key:(String.make 32 '0')
    ~nonce:("01" ^ String.make 14 '0')
    ~output0:("1663879eb3f2c9949e2388caa343d361" ^
              "bb132771245ae6d027ca9cb010dc1fa7" ^
              "178dc41f8278bc1f64b3f12769a24097" ^
              "f40d63a86366bdb36ac08abe60c07fe8")
    ~output1:("b057375c89144408cc744624f69f7f4c" ^
              "cbd93366c92fc4dfcada65f1b959d8c6" ^
              "4dfc50de711fb46416c2553cc60f21bb" ^
              "fd006491cb17888b4fb3521c4fdd8745")

let chacha20_128_4_test =
  test_chacha20
    ~key:(String.make 32 'f')
    ~nonce:(String.make 16 'f')
    ~output0:("992947c3966126a0e660a3e95db048de" ^
              "091fb9e0185b1e41e41015bb7ee50150" ^
              "399e4760b262f9d53f26d8dd19e56f5c" ^
              "506ae0c3619fa67fb0c408106d0203ee")
    ~output1:("40ea3cfa61fa32a2fda8d1238a2135d9" ^
              "d4178775240f99007064a6a7f0c731b6" ^
              "7c227c52ef796b6bed9f9059ba0614bc" ^
              "f6dd6e38917f3b150e576375be50ed67")

let chacha20_128_5_test =
  test_chacha20
    ~key:(String.make 32 '5')
    ~nonce:(String.make 16 '5')
    ~output0:("357d7d94f966778f5815a2051dcb0413" ^
              "3b26b0ead9f57dd09927837bc3067e4b" ^
              "6bf299ad81f7f50c8da83c7810bfc17b" ^
              "b6f4813ab6c326957045fd3fd5e19915")
    ~output1:("ec744a6b9bf8cbdcb36d8b6a5499c68a" ^
              "08ef7be6cc1e93f2f5bcd2cad4e47c18" ^
              "a3e5d94b5666382c6d130d822dd56aac" ^
              "b0f8195278e7b292495f09868ddf12cc")

let chacha20_128_6_test =
  test_chacha20
    ~key:(String.make 32 'a')
    ~nonce:(String.make 16 'a')
    ~output0:("fc79acbd58526103862776aab20f3b7d" ^
              "8d3149b2fab65766299316b6e5b16684" ^
              "de5de548c1b7d083efd9e3052319e0c6" ^
                "254141da04a6586df800f64d46b01c87")
    ~output1:("1f05bc67e07628ebe6f6865a2177e0b6" ^
              "6a558aa7cc1e8ff1a98d27f7071f8335" ^
              "efce4537bb0ef7b573b32f32765f2900" ^
              "7da53bba62e7a44d006f41eb28fe15d6")

let chacha20_128_7_test =
  test_chacha20
    ~key:"00112233445566778899aabbccddeeff"
    ~nonce:"0f1e2d3c4b5a6978"
    ~output0:("d1abf630467eb4f67f1cfb47cd626aae" ^
              "8afedbbe4ff8fc5fe9cfae307e74ed45" ^
              "1f1404425ad2b54569d5f18148939971" ^
              "abb8fafc88ce4ac7fe1c3d1f7a1eb7ca")
    ~output1:("e76ca87b61a9713541497760dd9ae059" ^
              "350cad0dcedfaa80a883119a1a6f987f" ^
              "d1ce91fd8ee0828034b411200a9745a2" ^
              "85554475d12afc04887fef3516d12a2c")

let chacha20_128_8_test =
  test_chacha20
    ~key:"c46ec1b18ce8a878725a37e780dfb735"
    ~nonce:"1ada31d5cf688221"
    ~output0:("826abdd84460e2e9349f0ef4af5b179b" ^
              "426e4b2d109a9c5bb44000ae51bea90a" ^
              "496beeef62a76850ff3f0402c4ddc99f" ^
              "6db07f151c1c0dfac2e56565d6289625")
    ~output1:("5b23132e7b469c7bfb88fa95d44ca5ae" ^
              "3e45e848a4108e98bad7a9eb15512784" ^
              "a6a9e6e591dce674120acaf9040ff50f" ^
              "f3ac30ccfb5e14204f5e4268b90a8804")

let chacha20_128_bit_tests = [
  "TC1: All zero key and IV.", `Quick, chacha20_128_1_test;
  "TC2: Single bit in key set. All zero IV.", `Quick, chacha20_128_2_test;
  "TC3: Single bit in IV set. All zero key.", `Quick, chacha20_128_3_test;
  "TC4: All bits in key and IV are set.", `Quick, chacha20_128_4_test;
  "TC5: Every even bit set in key and IV.", `Quick, chacha20_128_5_test;
  "TC6: Every odd bit set in key and IV.", `Quick, chacha20_128_6_test;
  "TC7: Sequence patterns in key and IV.", `Quick, chacha20_128_7_test;
  "TC8: key: 'All your base are belong to us!, IV: 'IETF2013'", `Quick, chacha20_128_8_test;
]

let chacha12_256_1_test =
  test_chacha12
    ~key:(String.make 64 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("9bf49a6a0755f953811fce125f2683d5" ^
              "0429c3bb49e074147e0089a52eae155f" ^
              "0564f879d27ae3c02ce82834acfa8c79" ^
              "3a629f2ca0de6919610be82f411326be")
    ~output1:("0bd58841203e74fe86fc71338ce0173d" ^
              "c628ebb719bdcbcc151585214cc089b4" ^
              "42258dcda14cf111c602b8971b8cc843" ^
              "e91e46ca905151c02744a6b017e69316")

let chacha12_256_2_test =
  test_chacha12
    ~key:("01" ^ String.make 62 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("12056e595d56b0f6eef090f0cd25a209" ^
              "49248c2790525d0f930218ff0b4ddd10" ^
              "a6002239d9a454e29e107a7d06fefdfe" ^
              "f0210feba044f9f29b1772c960dc29c0")
    ~output1:("0c7366c5cbc604240e665eb02a69372a" ^
              "7af979b26fbb78092ac7c4b88029a7c8" ^
              "54513bc217bbfc7d90432e308eba15af" ^
              "c65aeb48ef100d5601e6afba257117a9")

let chacha12_256_3_test =
  test_chacha12
    ~key:(String.make 64 '0')
    ~nonce:("01" ^ String.make 14 '0')
    ~output0:("64b8bdf87b828c4b6dbaf7ef698de03d" ^
              "f8b33f635714418f9836ade59be12969" ^
              "46c953a0f38ecffc9ecb98e81d5d99a5" ^
              "edfc8f9a0a45b9e41ef3b31f028f1d0f")
    ~output1:("559db4a7f222c442fe23b9a2596a8828" ^
              "5122ee4f1363896ea77ca150912ac723" ^
              "bff04b026a2f807e03b29c02077d7b06" ^
              "fc1ab9827c13c8013a6d83bd3b52a26f")

let chacha12_256_4_test =
  test_chacha12
    ~key:(String.make 64 'f')
    ~nonce:(String.make 16 'f')
    ~output0:("04bf88dae8e47a228fa47b7e6379434b" ^
              "a664a7d28f4dab84e5f8b464add20c3a" ^
              "caa69c5ab221a23a57eb5f345c96f4d1" ^
              "322d0a2ff7a9cd43401cd536639a615a")
    ~output1:("5c9429b55ca3c1b55354559669a154ac" ^
              "a46cd761c41ab8ace385363b95675f06" ^
              "8e18db5a673c11291bd4187892a9a3a3" ^
              "3514f3712b26c13026103298ed76bc9a")

let chacha12_256_5_test =
  test_chacha12
    ~key:(String.make 64 '5')
    ~nonce:(String.make 16 '5')
    ~output0:("a600f07727ff93f3da00dd74cc3e8bfb" ^
              "5ca7302f6a0a2944953de00450eecd40" ^
              "b860f66049f2eaed63b2ef39cc310d2c" ^
              "488f5d9a241b615dc0ab70f921b91b95")
    ~output1:("140eff4aa495ac61289b6bc57de07241" ^
              "9d09daa7a7243990daf348a8f2831e59" ^
              "7cf379b3b284f00bda27a4c68085374a" ^
              "8a5c38ded62d1141cae0bb838ddc2232")

let chacha12_256_6_test =
  test_chacha12
    ~key:(String.make 64 'a')
    ~nonce:(String.make 16 'a')
    ~output0:("856505b01d3b47aae03d6a97aa0f033a" ^
              "9adcc94377babd8608864fb3f625b6e3" ^
              "14f086158f9f725d811eeb953b7f7470" ^
              "76e4c3f639fa841fad6c9a709e621397")
    ~output1:("6dd6ee9b5e1e2e676b1c9e2b82c2e96c" ^
              "1648437bff2f0126b74e8ce0a9b06d17" ^
              "20ac0b6f09086f28bc201587f0535ed9" ^
              "385270d08b4a9382f18f82dbde18210e")

let chacha12_256_7_test =
  test_chacha12
    ~key:"00112233445566778899aabbccddeeffffeeddccbbaa99887766554433221100"
    ~nonce:"0f1e2d3c4b5a6978"
    ~output0:("7ed12a3a63912ae941ba6d4c0d5e862e" ^
              "568b0e5589346935505f064b8c2698db" ^
              "f7d850667d8e67be639f3b4f6a16f92e" ^
              "65ea80f6c7429445da1fc2c1b9365040")
    ~output1:("e32e50c4106f3b3da1ce7ccb1e7140b1" ^
              "53493c0f3ad9a9bcff077ec4596f1d0f" ^
              "29bf9cbaa502820f732af5a93c49eee3" ^
              "3d1c4f12af3b4297af91fe41ea9e94a2")

let chacha12_256_8_test =
  test_chacha12
    ~key:"c46ec1b18ce8a878725a37e780dfb7351f68ed2e194c79fbc6aebee1a667975d"
    ~nonce:"1ada31d5cf688221"
    ~output0:("1482072784bc6d06b4e73bdc118bc010" ^
              "3c7976786ca918e06986aa251f7e9cc1" ^
              "b2749a0a16ee83b4242d2e99b08d7c20" ^
              "092b80bc466c87283b61b1b39d0ffbab")
    ~output1:("d94b116bc1ebdb329b9e4f620db69554" ^
              "4a8e3d9b68473d0c975a46ad966ed631" ^
              "e42aff530ad5eac7d8047adfa1e5113c" ^
              "91f3e3b883f1d189ac1c8fe07ba5a42b")

let chacha12_256_bit_tests = [
  "TC1: All zero key and IV.", `Quick, chacha12_256_1_test;
  "TC2: Single bit in key set. All zero IV.", `Quick, chacha12_256_2_test;
  "TC3: Single bit in IV set. All zero key.", `Quick, chacha12_256_3_test;
  "TC4: All bits in key and IV are set.", `Quick, chacha12_256_4_test;
  "TC5: Every even bit set in key and IV.", `Quick, chacha12_256_5_test;
  "TC6: Every odd bit set in key and IV.", `Quick, chacha12_256_6_test;
  "TC7: Sequence patterns in key and IV.", `Quick, chacha12_256_7_test;
  "TC8: key: 'All your base are belong to us!, IV: 'IETF2013'", `Quick, chacha12_256_8_test;
]

let chacha12_128_1_test =
  test_chacha12
    ~key:(String.make 32 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("e1047ba9476bf8ff312c01b4345a7d8c" ^
              "a5792b0ad467313f1dc412b5fdce3241" ^
              "0dea8b68bd774c36a920f092a04d3f95" ^
              "274fbeff97bc8491fcef37f85970b450")
    ~output1:("1d43b61a8f7e19fceddef368ae6bfb11" ^
              "101bd9fd3e4d127de30db2db1b472e76" ^
              "426803a45e15b962751986ef1d9d50f5" ^
              "98a5dcdc9fa529a28357991e784ea20f")

let chacha12_128_2_test =
  test_chacha12
    ~key:("01" ^ String.make 30 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("2a865a3b8999fa83ae8aacf33fc6be4f" ^
              "32c8aa9762738d26963270052f4eef8b" ^
              "86af758f7867560af6d0eeb973b5542b" ^
              "b24c8abceac8b1f36d026963d6c8a9b2")
    ~output1:("d82ce0cad37d51b1052c33144a30a823" ^
              "9c9fca6284ac5ea750bebb2d224dbb39" ^
              "aa4e7acd511f8cef15a5c490590e38e9" ^
              "6397c06cd21c389cb8b1159c240c9c0e")

let chacha12_128_3_test =
  test_chacha12
    ~key:(String.make 32 '0')
    ~nonce:("01" ^ String.make 14 '0')
    ~output0:("91cdb2f180bc89cfe86b8b6871cd6b3a" ^
              "f61abf6eba01635db619c40a0b2e19ed" ^
              "fa8ce5a9bd7f53cc2c9bcfea181e9754" ^
              "a9e245731f658cc282c2ae1cab1ae02c")
    ~output1:("4366d288f0f88e001680bc02f1b19a96" ^
              "37a261a13bd83e312f3758ea89ba7222" ^
              "3d65b1cd40cea478b20f4e2bbb9a98ea" ^
              "05fabc05f86df9a289326d379afb99b9")

let chacha12_128_4_test =
  test_chacha12
    ~key:(String.make 32 'f')
    ~nonce:(String.make 16 'f')
    ~output0:("60e349e60c38b328c4baab90d44a7c72" ^
              "7662770d36350d65a1433bd92b00ecf4" ^
              "83d5597d7a616258ec3c5d5b30e1c5c8" ^
              "5c5dfe2f92423b8e36870f3185b6add9")
    ~output1:("f34dab6c2bc551898fbdcdfc783f0917" ^
              "1cc8b59a8b2852983c3a9b91d29b5761" ^
              "12464a9d8e050263e989906f42c7efca" ^
              "c8a70a85bb7ff2211273fbd4cad96142")

let chacha12_128_5_test =
  test_chacha12
    ~key:(String.make 32 '5')
    ~nonce:(String.make 16 '5')
    ~output0:("90ec7a49ee0b20a808af3d463c1fac6c" ^
              "2a7c897ce8f6e60d793b62ddbebcf980" ^
              "ac917f091e52952db063b1d2b947de04" ^
              "aac087190ca99a35b5ea501eb535d570")
    ~output1:("8f78ccea3d9452584450101ac495cd16" ^
              "6efd69426b47fa6e8e788921f29e3d54" ^
              "7364b952913173a5bac500e89d8c66c6" ^
              "ce51ed626d0da8dc94deec92125ea48d")

let chacha12_128_6_test =
  test_chacha12
    ~key:(String.make 32 'a')
    ~nonce:(String.make 16 'a')
    ~output0:("057fe84fead13c24b76bb2a6fdde66f2" ^
              "688e8eb6268275c22c6bcb90b85616d7" ^
              "fe4d3193a1036b70d7fb864f01453641" ^
              "851029ecdb60ac3879f56496f16213f4")
    ~output1:("e9e61945b8d854a1749a7c1fc5fb584d" ^
              "cfc68c558e6efe045b51d513ebeb093f" ^
              "be91d7ba36dc6f0c8c7cfa66654ad99d" ^
              "64c342bb3047368b7edddf836c7253cc")

let chacha12_128_7_test =
  test_chacha12
    ~key:"00112233445566778899aabbccddeeff"
    ~nonce:"0f1e2d3c4b5a6978"
    ~output0:("5eddc2d9428fceeec50a52a964eae0ff" ^
              "b04b2de006a9b04cff368ffa921116b2" ^
              "e8e264babd2efa0de43ef2e3b6d065e8" ^
              "f7c0a17837b0a40eb0e2c7a3742c8753")
    ~output1:("ede5f3f6d19be554675e506a775c63f0" ^
              "94d4965c319319dcd7506f457b117b84" ^
              "b10b246e956c2da8898a656ceef3f7b7" ^
              "1645b19f701db84485ce5121f0f617ef")

let chacha12_128_8_test =
  test_chacha12
    ~key:"c46ec1b18ce8a878725a37e780dfb735"
    ~nonce:"1ada31d5cf688221"
    ~output0:("b02bd81eb55c8f68b5e9ca4e307079bc" ^
              "225bd22007eddc6702801820709ce098" ^
              "07046a0d2aa552bfdbb49466176d56e3" ^
              "2d519e10f5ad5f2746e241e09bdf9959")
    ~output1:("17be0873edde9af5b86246441ce41019" ^
              "5baede41f8bdab6ad253226382ee383e" ^
              "3472f945a5e6bd628c7a582bcf8f8998" ^
              "70596a58dab83b51a50c7dbb4f3e6e76")

let chacha12_128_bit_tests = [
  "TC1: All zero key and IV.", `Quick, chacha12_128_1_test;
  "TC2: Single bit in key set. All zero IV.", `Quick, chacha12_128_2_test;
  "TC3: Single bit in IV set. All zero key.", `Quick, chacha12_128_3_test;
  "TC4: All bits in key and IV are set.", `Quick, chacha12_128_4_test;
  "TC5: Every even bit set in key and IV.", `Quick, chacha12_128_5_test;
  "TC6: Every odd bit set in key and IV.", `Quick, chacha12_128_6_test;
  "TC7: Sequence patterns in key and IV.", `Quick, chacha12_128_7_test;
  "TC8: key: 'All your base are belong to us!, IV: 'IETF2013'", `Quick, chacha12_128_8_test;
]

let chacha8_256_1_test =
  test_chacha8
    ~key:(String.make 64 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("3e00ef2f895f40d67f5bb8e81f09a5a1" ^
              "2c840ec3ce9a7f3b181be188ef711a1e" ^
              "984ce172b9216f419f445367456d5619" ^
              "314a42a3da86b001387bfdb80e0cfe42")
    ~output1:("d2aefa0deaa5c151bf0adb6c01f2a5ad" ^
              "c0fd581259f9a2aadcf20f8fd566a26b" ^
              "5032ec38bbc5da98ee0c6f568b872a65" ^
              "a08abf251deb21bb4b56e5d8821e68aa")

let chacha8_256_2_test =
  test_chacha8
    ~key:("01" ^ String.make 62 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("cf5ee9a0494aa9613e05d5ed725b804b" ^
              "12f4a465ee635acc3a311de8740489ea" ^
              "289d04f43c7518db56eb4433e498a123" ^
              "8cd8464d3763ddbb9222ee3bd8fae3c8")
    ~output1:("b4355a7d93dd8867089ee643558b9575" ^
              "4efa2bd1a8a1e2d75bcdb32015542638" ^
              "291941feb49965587c4fdfe219cf0ec1" ^
              "32a6cd4dc067392e67982fe53278c0b4")

let chacha8_256_3_test =
  test_chacha8
    ~key:(String.make 64 '0')
    ~nonce:("01" ^ String.make 14 '0')
    ~output0:("2b8f4bb3798306ca5130d47c4f8d4ed1" ^
              "3aa0edccc1be6942090faeeca0d7599b" ^
              "7ff0fe616bb25aa0153ad6fdc88b9549" ^
              "03c22426d478b97b22b8f9b1db00cf06")
    ~output1:("470bdffbc488a8b7c701ebf4061d75c5" ^
              "969186497c95367809afa80bd843b040" ^
              "a79abc6e73a91757f1db73c8eacfa543" ^
              "b38f289d065ab2f3032d377b8c37fe46")

let chacha8_256_4_test =
  test_chacha8
    ~key:(String.make 64 'f')
    ~nonce:(String.make 16 'f')
    ~output0:("e163bbf8c9a739d18925ee8362dad2cd" ^
              "c973df05225afb2aa26396f2a9849a4a" ^
              "445e0547d31c1623c537df4ba85c70a9" ^
              "884a35bcbf3dfab077e98b0f68135f54")
    ~output1:("81d4933f8b322ac0cd762c27235ce2b3" ^
              "1534e0244a9a2f1fd5e94498d47ff108" ^
              "790c009cf9e1a348032a7694cb28024c" ^
              "d96d3498361edb1785af752d187ab54b")

let chacha8_256_5_test =
  test_chacha8
    ~key:(String.make 64 '5')
    ~nonce:(String.make 16 '5')
    ~output0:("7cb78214e4d3465b6dc62cf7a1538c88" ^
              "996952b4fb72cb6105f1243ce3442e29" ^
              "75a59ebcd2b2a598290d7538491fe65b" ^
              "dbfefd060d88798120a70d049dc2677d")
    ~output1:("d48ff5a2513e497a5d54802d7484c4f1" ^
              "083944d8d0d14d6482ce09f7e5ebf20b" ^
              "29807d62c31874d02f5d3cc85381a745" ^
              "ecbc60525205e300a76961bfe51ac07c")

let chacha8_256_6_test =
  test_chacha8
    ~key:(String.make 64 'a')
    ~nonce:(String.make 16 'a')
    ~output0:("40f9ab86c8f9a1a0cdc05a75e5531b61" ^
              "2d71ef7f0cf9e387df6ed6972f0aae21" ^
              "311aa581f816c90e8a99de990b6b95aa" ^
              "c92450f4e112712667b804c99e9c6eda")
    ~output1:("f8d144f560c8c0ea36880d3b77874c9a" ^
              "9103d147f6ded386284801a4ee158e5e" ^
              "a4f9c093fc55fd344c33349dc5b699e2" ^
              "1dc83b4296f92ee3ecabf3d51f95fe3f")

let chacha8_256_7_test =
  test_chacha8
    ~key:"00112233445566778899aabbccddeeffffeeddccbbaa99887766554433221100"
    ~nonce:"0f1e2d3c4b5a6978"
    ~output0:("db43ad9d1e842d1272e4530e276b3f56" ^
              "8f8859b3f7cf6d9d2c74fa53808cb515" ^
              "7a8ebf46ad3dcc4b6c7dadde131784b0" ^
              "120e0e22f6d5f9ffa7407d4a21b695d9")
    ~output1:("c5dd30bf55612fab9bdd118920c19816" ^
              "470c7f5dcd42325dbbed8c57a56281c1" ^
              "44cb0f03e81b3004624e0650a1ce5afa" ^
              "f9a7cd8163f6dbd72602257dd96e471e")

let chacha8_256_8_test =
  test_chacha8
    ~key:"c46ec1b18ce8a878725a37e780dfb7351f68ed2e194c79fbc6aebee1a667975d"
    ~nonce:"1ada31d5cf688221"
    ~output0:("838751b42d8ddd8a3d77f48825a2ba75" ^
              "2cf4047cb308a5978ef274973be374c9" ^
              "6ad848065871417b08f034e681fe46a9" ^
              "3f7d5c61d1306614d4aaf257a7cff08b")
    ~output1:("16f2fda170cc18a4b58a2667ed962774" ^
              "af792a6e7f3c77992540711a7a136d7e" ^
              "8a2f8d3f93816709d45a3fa5f8ce72fd" ^
              "e15be7b841acba3a2abd557228d9fe4f")

let chacha8_256_bit_tests = [
  "TC1: All zero key and IV.", `Quick, chacha8_256_1_test;
  "TC2: Single bit in key set. All zero IV.", `Quick, chacha8_256_2_test;
  "TC3: Single bit in IV set. All zero key.", `Quick, chacha8_256_3_test;
  "TC4: All bits in key and IV are set.", `Quick, chacha8_256_4_test;
  "TC5: Every even bit set in key and IV.", `Quick, chacha8_256_5_test;
  "TC6: Every odd bit set in key and IV.", `Quick, chacha8_256_6_test;
  "TC7: Sequence patterns in key and IV.", `Quick, chacha8_256_7_test;
  "TC8: key: 'All your base are belong to us!, IV: 'IETF2013'", `Quick, chacha8_256_8_test;
]

let chacha8_128_1_test =
  test_chacha8
    ~key:(String.make 32 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("e28a5fa4a67f8c5defed3e6fb7303486" ^
              "aa8427d31419a729572d777953491120" ^
              "b64ab8e72b8deb85cd6aea7cb6089a10" ^
              "1824beeb08814a428aab1fa2c816081b")
    ~output1:("8a26af448a1ba906368fd8c83831c18c" ^
              "ec8ced811a028e675b8d2be8fce08116" ^
              "5ceae9f1d1b7a975497749480569ceb8" ^
              "3de6a0a587d4984f19925f5d338e430d")

let chacha8_128_2_test =
  test_chacha8
    ~key:("01" ^ String.make 30 '0')
    ~nonce:(String.make 16 '0')
    ~output0:("03a7669888605a0765e8357475e58673" ^
              "f94fc8161da76c2a3aa2f3caf9fe5449" ^
              "e0fcf38eb882656af83d430d410927d5" ^
              "5c972ac4c92ab9da3713e19f761eaa14")
    ~output1:("7138c25c8a7ce3d5e7546746ffd2e351" ^
              "5ce6a4b1b2d3f380138668ed39fa92f8" ^
              "a1aee36258e05fae6f566673511765fd" ^
              "b59e05163d55a708c5f9bc45045124cb")

let chacha8_128_3_test =
  test_chacha8
    ~key:(String.make 32 '0')
    ~nonce:("01" ^ String.make 14 '0')
    ~output0:("25f5bec6683916ff44bccd12d102e692" ^
              "176663f4cac53e719509ca74b6b2eec8" ^
              "5da4236fb29902012adc8f0d86c8187d" ^
              "25cd1c486966930d0204c4ee88a6ab35")
    ~output1:("5a6c9976c7bc6e78baf3108c5364ef42" ^
              "b93b35d2694d2ddf72a4fc7ecdb968fc" ^
              "fe16bedb8d48102fb54f1ce3636e914c" ^
              "0e2dadc7caa2ab1929733a9263325e72")

let chacha8_128_4_test =
  test_chacha8
    ~key:(String.make 32 'f')
    ~nonce:(String.make 16 'f')
    ~output0:("2204d5b81ce662193e00966034f91302" ^
              "f14a3fb047f58b6e6ef0d72113230416" ^
              "3e0fb640d76ff9c3b9cd99996e6e38fa" ^
              "d13f0e31c82244d33abbc1b11e8bf12d")
    ~output1:("9a81d78e9e56604ddfae136921f51c9d" ^
              "81ae15119db8e756dd28024493ee571d" ^
              "363ae4bbcd6e7d300f99d2673aeb92cc" ^
              "fc6e43a38dc31bacd66b28f17b22b28a")

let chacha8_128_5_test =
  test_chacha8
    ~key:(String.make 32 '5')
    ~nonce:(String.make 16 '5')
    ~output0:("f0a23bc36270e18ed0691dc384374b9b" ^
              "2c5cb60110a03f56fa48a9fbbad961aa" ^
              "6bab4d892e96261b6f1a0919514ae56f" ^
              "86e066e17c71a4176ac684af1c931996")
    ~output1:("950f754e728bd061d176ecf571c62a5e" ^
              "a5c776697b3193d3ea94cf17d7f0a14e" ^
              "504859d1a67c248ab298be3bb7eded3a" ^
              "23f61b6c5bd1a5a4cfc84bfc3d295ac5")

let chacha8_128_6_test =
  test_chacha8
    ~key:(String.make 32 'a')
    ~nonce:(String.make 16 'a')
    ~output0:("312d95c0bc38eff4942db2d50bdc500a" ^
              "30641ef7132db1a8ae838b3bea3a7ab0" ^
              "3815d7a4cc09dbf5882a3433d743aced" ^
              "48136ebab73299506855c0f5437a36c6")
    ~output1:("ef5ad3d6a4f6c35d9d66c2e34005b91b" ^
              "bbe3099e135a00ce2f700745be625319" ^
              "5824d4b19f69731b6177e624358c7977" ^
              "e67552f519b470e3f7a8ec965dc3beda")

let chacha8_128_7_test =
  test_chacha8
    ~key:"00112233445566778899aabbccddeeff"
    ~nonce:"0f1e2d3c4b5a6978"
    ~output0:("29560d280b4528400a8f4b795369fb3a" ^
              "01105599e9f1ed58279cfc9ece2dc5f9" ^
              "9f1c2e52c98238f542a5c0a881d850b6" ^
              "15d3acd9fbdb026e9368565da50e0d49")
    ~output1:("dd5be8ef74248b3e251d965d8fcb21e7" ^
              "cfe204d4007806fbee3ce94c74bfbad2" ^
              "c11c621ba048147c5caa94d182ccff6f" ^
              "d5cf44adf96e3d68281bb49676af87e7")

let chacha8_128_8_test =
  test_chacha8
    ~key:"c46ec1b18ce8a878725a37e780dfb735"
    ~nonce:"1ada31d5cf688221"
    ~output0:("6a870108859f679118f3e205e2a56a68" ^
              "26ef5a60a4102ac8d4770059fcb7c7ba" ^
              "e02f5ce004a6bfbbea53014dd82107c0" ^
              "aa1c7ce11b7d78f2d50bd3602bbd2594")
    ~output1:("0560bb6a84289e0b38f5dd21d6ef6d77" ^
              "37e3ec0fb772da2c71c2397762e5dbbb" ^
              "f449e3d1639ccbfa3e069c4d871ed639" ^
              "5b22aaf35c8da6de2dec3d77880da8e8")

let chacha8_128_bit_tests = [
  "TC1: All zero key and IV.", `Quick, chacha8_128_1_test;
  "TC2: Single bit in key set. All zero IV.", `Quick, chacha8_128_2_test;
  "TC3: Single bit in IV set. All zero key.", `Quick, chacha8_128_3_test;
  "TC4: All bits in key and IV are set.", `Quick, chacha8_128_4_test;
  "TC5: Every even bit set in key and IV.", `Quick, chacha8_128_5_test;
  "TC6: Every odd bit set in key and IV.", `Quick, chacha8_128_6_test;
  "TC7: Sequence patterns in key and IV.", `Quick, chacha8_128_7_test;
  "TC8: key: 'All your base are belong to us!, IV: 'IETF2013'", `Quick, chacha8_128_8_test;
]

let () =
  Alcotest.run "ChaCha Tests" [
    "ChaCha20 256bit key tests", chacha20_256_bit_tests;
    "ChaCha20 128bit key tests", chacha20_128_bit_tests;
    "ChaCha12 256bit key tests", chacha12_256_bit_tests;
    "ChaCha12 128bit key tests", chacha12_128_bit_tests;
    "ChaCha8 256bit key tests", chacha8_256_bit_tests;
    "ChaCha8 128bit key tests", chacha8_128_bit_tests;
  ]
