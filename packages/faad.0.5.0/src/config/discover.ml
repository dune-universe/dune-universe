module C = Configurator.V1

let faad_test_code = {|
#include <neaacdec.h>

int main()
{
  NeAACDecHandle hAac = NeAACDecOpen();
  unsigned char input[1024];
  size_t input_size = 0;
  unsigned long samplerate;
  unsigned char channels;

  char err = NeAACDecInit(hAac, input, input_size, &samplerate, &channels);

  return 0;
}
|}

let () =
  C.main ~name:"has_faad" (fun c ->
    let has_faad = C.c_test c faad_test_code ~link_flags:["-lfaad -lm"] in

    C.C_define.gen_header_file c ~fname:"config.h"
      [ "HAS_FAAD", Switch has_faad ]);
