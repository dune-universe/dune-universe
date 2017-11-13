
type 'a t =
    { default_value : 'a ;
      mutable realsize : int ;
      mutable tab : 'a array }
      
let create n v =
  { default_value = v ;
    realsize = 0 ;
    tab = Array.make n v }

let copy t = 
  { default_value = t.default_value ;
    realsize = t.realsize ;
    tab = Array.copy t.tab }

let get t index = 
  if index < 0 then failwith "extArray.get, negative index."
  else if index >= t.realsize then t.default_value
  else t.tab.(index)

let set t index v =

  let len = Array.length t.tab in
  if index >= len then
    begin
      (* Resize *)
      let newarray = Array.make (max (index+1) (2 * len)) t.default_value in
      Array.blit t.tab 0 newarray 0 len ;
      t.tab <- newarray ;
    end ;

  t.tab.(index) <- v ;
  t.realsize <- max t.realsize (index+1) ;
  ()

let size t = t.realsize

let fold t acu f =
  let akk = ref acu in
  for i = 0 to t.realsize - 1 do
    akk := f i t.tab.(i) !akk ;
  done ;
  !akk

let iter t f =
  for i = 0 to t.realsize - 1 do
    f i t.tab.(i) ;
  done
  

