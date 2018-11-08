open Diagram
open AcgData.Environment
open Logic.Lambda.Lambda
open Show_exts


module Lambda_show (T : Show_text_sig) : sig

  val fix : (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)
  val parenthesize_d : diagram * bool -> diagram
  val term_to_diagram_open : open_pp
  val term_to_diagram : term -> consts -> diagram

end


module Make (E : Environment_sig)
            (T : Show_text_sig)
            (C : Show_colors_sig)
            (Emb : Show_embellish_sig) : sig

  type lexicon = E.Lexicon.t
  type term = E.Signature1.term

  val realize_diagram : term -> lexicon list -> Rendering_config.config -> diagram

end
