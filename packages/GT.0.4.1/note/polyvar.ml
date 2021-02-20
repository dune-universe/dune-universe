type 'a p = P of 'a * 'a

class virtual ['i, 's] p_t =
  object (self)
    method virtual m_P : 'a . 'i -> (('i -> 'a p -> 's) * 'a p) -> (('i -> 'a -> 's) * 'a) -> (('i -> 'a -> 's) * 'a) -> 's
    method virtual t_p : 'a . ('i -> 'a -> 's) -> 'i -> 'a p -> 's
  end

let rec transform_p : 'i 's 'a . ('i -> 'a -> 's) -> ('i, 's) #p_t -> 'i -> 'a p -> 's = fun fa t acc x ->
  let self = transform_p fa t in
  match x with
  | P (a, b) -> t#m_P acc (self, x) (fa, a) (fa, b) 

class show_p' env =
  object (self)
    inherit [unit, string] p_t
    method m_P () (_, _) (fa, a) (fb, b) = Printf.sprintf "P (%s, %s)" (fa () a) (fb () b)
    method t_p fa _ p = transform_p fa self () p
  end

class show_p = show_p' (object end)

type 'a q = N | C of 'a * 'a q

class virtual ['i, 's] q_t = 
  object (self)
    method virtual m_N : 'a . 'i -> (('i -> 'a q -> 's) * 'a q) -> 's
    method virtual m_C : 'a . 'i -> (('i -> 'a q -> 's) * 'a q) -> (('i -> 'a -> 's) * 'a) -> (('i -> 'a q -> 's) * 'a q) -> 's 
    method virtual t_q : 'a . ('i -> 'a -> 's) -> 'i -> 'a q -> 's
  end

let rec transform_q : 'i 's 'a . ('i -> 'a -> 's) -> ('i, 's) #q_t -> 'i -> 'a q -> 's = fun fa t acc x ->
  let self = transform_q fa t in
  match x with
  | N -> t#m_N acc (self, x)
  | C (a, b) -> t#m_C acc (self, x) (fa, a) (self, b) 

class show_q' env = 
  object (self)
    inherit [unit, string] q_t
    method m_N _ _ = "N"
    method m_C _ _ (fa, a) (fb, b) = Printf.sprintf "C (%s, %s)" (fa () a) (fb () b)
    method t_q fa _ q = transform_q fa self () q
  end

class show_q = show_q' (object end)

type ('a, 'b) t = A of 'a * ('b, 'a) t p | B of 'b * ('a, 'b) t q 

class virtual ['i, 's] t_t =
  object (self)
    method virtual m_A : 'a 'b . 'i -> (('i -> ('a, 'b) t -> 's) * ('a, 'b) t) -> (('i -> 'a -> 's) * 'a) -> (('i -> ('b, 'a) t p -> 's) * ('b, 'a) t p) -> 's 
    method virtual m_B : 'a 'b . 'i -> (('i -> ('a, 'b) t -> 's) * ('a, 'b) t) -> (('i -> 'b -> 's) * 'b) -> (('i -> ('a, 'b) t q -> 's) * ('a, 'b) t q) -> 's 
    method virtual t_p : 'a . ('i -> 'a -> 's) -> 'i -> 'a p -> 's
    method virtual t_q : 'a . ('i -> 'a -> 's) -> 'i -> 'a q -> 's
    method virtual t_t : 'a 'b . ('i -> 'a -> 's) -> ('i -> 'b -> 's) -> 'i -> ('a, 'b) t -> 's
  end

let rec transform_t : 'i 's 'a 'b . ('i -> 'a -> 's) -> ('i -> 'b -> 's) -> ('i, 's) #t_t -> 'i -> ('a, 'b) t -> 's = fun fa fb t acc x ->
  let self     = transform_t fa fb t in 
  let self_b_a = transform_t fb fa t in 
  match x with
  | A (a, p) -> t#m_A acc (self, x) (fa, a) (t#t_p self_b_a, p)
  | B (b, q) -> t#m_B acc (self, x) (fb, b) (t#t_q self, q)

class show_t' (env : < t_p : 'a . (unit -> 'a -> string) -> unit -> 'a p -> string; 
                       t_q : 'a . (unit -> 'a -> string) -> unit -> 'a q -> string; 
                      ..
                     >) =
  object (self)
    inherit [unit, string] t_t
    method m_A _ _ (fa, a) (fb, b) = Printf.sprintf "A (%s, %s)" (fa () a) (fb () b)
    method m_B _ _ (fa, a) (fb, b) = Printf.sprintf "B (%s, %s)" (fa () a) (fb () b)
    method t_p = env#t_p
    method t_q = env#t_q 
    method t_t fa fb _ t = transform_t fa fb self () t
  end

class show_t = show_t' (object inherit show_p inherit show_q end)

let _ = 
  let x = B ("1", C (A (2, P (B (3, N), B (4, N))), N)) in
  Printf.printf "%s\n" (transform_t (fun () x -> string_of_int x) (fun () x -> x) (new show_t) () x)

