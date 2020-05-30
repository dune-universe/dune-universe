open Plebeia.Internal

let test st = 
  for i = 1 to Segment.max_length do
    let seg = Test_utils.random_segment ~length:i st in
    assert (Segment.equal (Segment.decode (Segment.encode seg)) seg)
  done

let () = test (Random.State.make_self_init ())
    
    
