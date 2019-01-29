open TestFramework;

open Rhythm;

describe("Math", ({test}) => {
  test("log2", ({expect}) => {
    /* Tests values 2^k and (2^k)-1 for k in [1, 32] */
    expect.int(Math.log2(1)).toBe(0);
    expect.int(Math.log2(2)).toBe(1);
    expect.int(Math.log2(3)).toBe(1);
    expect.int(Math.log2(4)).toBe(2);
    expect.int(Math.log2(7)).toBe(2);
    expect.int(Math.log2(8)).toBe(3);
    expect.int(Math.log2(15)).toBe(3);
    expect.int(Math.log2(16)).toBe(4);
    expect.int(Math.log2(31)).toBe(4);
    expect.int(Math.log2(32)).toBe(5);
    expect.int(Math.log2(63)).toBe(5);
    expect.int(Math.log2(64)).toBe(6);
    expect.int(Math.log2(127)).toBe(6);
    expect.int(Math.log2(128)).toBe(7);
    expect.int(Math.log2(255)).toBe(7);
    expect.int(Math.log2(256)).toBe(8);
    expect.int(Math.log2(511)).toBe(8);
    expect.int(Math.log2(512)).toBe(9);
    expect.int(Math.log2(1_023)).toBe(9);
    expect.int(Math.log2(1_024)).toBe(10);
    expect.int(Math.log2(2_047)).toBe(10);
    expect.int(Math.log2(2_048)).toBe(11);
    expect.int(Math.log2(4_095)).toBe(11);
    expect.int(Math.log2(4_096)).toBe(12);
    expect.int(Math.log2(8_191)).toBe(12);
    expect.int(Math.log2(8_192)).toBe(13);
    expect.int(Math.log2(16_383)).toBe(13);
    expect.int(Math.log2(16_384)).toBe(14);
    expect.int(Math.log2(32_767)).toBe(14);
    expect.int(Math.log2(32_768)).toBe(15);
    expect.int(Math.log2(65_535)).toBe(15);
    expect.int(Math.log2(65_536)).toBe(16);
    expect.int(Math.log2(131_071)).toBe(16);
    expect.int(Math.log2(131_072)).toBe(17);
    expect.int(Math.log2(262_143)).toBe(17);
    expect.int(Math.log2(262_144)).toBe(18);
    expect.int(Math.log2(524_287)).toBe(18);
    expect.int(Math.log2(524_288)).toBe(19);
    expect.int(Math.log2(1_048_575)).toBe(19);
    expect.int(Math.log2(1_048_576)).toBe(20);
    expect.int(Math.log2(2_097_151)).toBe(20);
    expect.int(Math.log2(2_097_152)).toBe(21);
    expect.int(Math.log2(4_194_303)).toBe(21);
    expect.int(Math.log2(4_194_304)).toBe(22);
    expect.int(Math.log2(8_388_607)).toBe(22);
    expect.int(Math.log2(8_388_608)).toBe(23);
    expect.int(Math.log2(16_777_215)).toBe(23);
    expect.int(Math.log2(16_777_216)).toBe(24);
    expect.int(Math.log2(33_554_431)).toBe(24);
    expect.int(Math.log2(33_554_432)).toBe(25);
    expect.int(Math.log2(67_108_863)).toBe(25);
    expect.int(Math.log2(67_108_864)).toBe(26);
    expect.int(Math.log2(134_217_727)).toBe(26);
    expect.int(Math.log2(134_217_728)).toBe(27);
    expect.int(Math.log2(268_435_455)).toBe(27);
    expect.int(Math.log2(268_435_456)).toBe(28);
    expect.int(Math.log2(536_870_911)).toBe(28);
    expect.int(Math.log2(536_870_912)).toBe(29);
    expect.int(Math.log2(1_073_741_823)).toBe(29);
    expect.int(Math.log2(1_073_741_824)).toBe(30);
    expect.int(Math.log2(2_147_483_647)).toBe(30);
    expect.int(Math.log2(2_147_483_648)).toBe(31);
    expect.int(Math.log2(4_294_967_295)).toBe(31);
    expect.int(Math.log2(4_294_967_296)).toBe(32);
  });

  test("log2 exceptions", ({expect}) => {
    expect.fn(() => Math.log2(-1)).toThrowException(
      Exceptions.Imaginary("log2"),
    );
    expect.fn(() => Math.log2(0)).toThrowException(
      Exceptions.Undefined("log2"),
    );
  });
});
