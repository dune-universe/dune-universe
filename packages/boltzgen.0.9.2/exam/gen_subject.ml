open Question_type
open Printf

let square ?i b =
  let sq = match b with
      true -> "\\rlap{\\raisebox{0.3ex}{\\hspace{0.4ex}\\tiny \\ding{52}}}"
    | false -> "" in
  let nb = match i with
      Some i -> "\\rlap{\\raisebox{0.3ex}{\\hspace{0.4ex}\\tiny "^(string_of_int i)^"}}"
    | None -> "" in
  sq^nb^"$\\square$"

let remove_strange = Str.regexp_string "&#10;"

let checkmark corrige f q =
  q.proposed_answer
  |> Array.iteri (fun i pa2 ->
         let pa = Str.global_replace remove_strange "\n" pa2 in
         let a = qcm_of_int i in
         if i>0 then fprintf f " \\qquad ";
         let b = corrige && string_of_qcm a = q.answer in
         fprintf f "%s %s : %s" (string_of_qcm a) (square b) pa
       )

let checkmark_list corrige f q =
  q.proposed_answer
  |> Array.iteri (fun i pa2 ->
         let pa = Str.global_replace remove_strange "\n" pa2 in
         if i>0 then fprintf f " \\qquad ";
         let b = corrige && List.mem (i+1) (qlist_of_string q.answer) in
         fprintf f "{%s~%s~:~%s}" (string_of_int (i+1)) (square b) pa
       )


let rec replacefn fn so =
  let s = Str.global_replace remove_strange "\n" so in
  let n = String.length s in
  match String.index_opt s '@' with
    Some i when i<(n-1) && s.[i+1]='?' -> (String.sub s 0 i)^(fn)^(replacefn fn (String.sub s (i+2) (n-i-2)))
  | Some i -> (String.sub s 0 (i+1))^(replacefn fn (String.sub s (i+1) (n-i-1)))
  | None -> s
;;

let escape_question_text fqi f s =
   let qtext = replacefn fqi s in
   fprintf f "\\\\ %s %s\n"
     qtext (if qtext.[String.length qtext -1] ='}' then "" else "\\\\" )


let gen_question ?(corrige=false) f id = function
  | Value q ->
     fprintf f "\\begin{qu}\n";
     let fqi = sprintf "fq_%i" id in
     (match q.rtype with
        Type.Name ("qcm",_) ->
         fprintf f "Écrire la valeur \\co{%s : %s} répondant à : %a"
           fqi (Type.string_of_compo q.rtype) (escape_question_text fqi) q.text ;
         (*fprintf f "\\ennonceQCM{%s,%s,%s}" fqi (Type.string_of_compo q.rtype) (replacefn fqi q.text);*)
         fprintf f "\\vspace{-0.3cm}\n";
         let mark = checkmark corrige in
         fprintf f "\\begin{center} %a \\end{center}" mark q
      | Type.Name ("qlist",_) ->
         fprintf f "Écrire la valeur \\co{%s : int list} contenant les propositions répondant à l'énoncé : %a" fqi
           (escape_question_text fqi) q.text;
         fprintf f "\\vspace{-0.3cm}\n";
         let mark = checkmark_list corrige in
         fprintf f "\\begin{center} %a \\end{center}" mark q
      | _ ->
         fprintf f "Écrire la fonction \\co{%a} %s \n" Type.print_func (Type.deano_func fqi q.rtype) (replacefn fqi q.text);
         if corrige then (
           fprintf f "test effort : %i" q.test_effort;
           fprintf f "\\begin{lstlisting}\n%s \\end{lstlisting}" q.answer );
     );
     fprintf f "\\end{qu}\n"
  | Type qt ->
     fprintf f "\\begin{qu}\n";
     let fqi = sprintf "tq_%i" id in
     fprintf f "Écrire le type  \\co{%s} répondant à l'énoncé : %a" fqi
       (escape_question_text fqi) qt.text;
     if corrige then (
       fprintf f "\\begin{lstlisting}\ntype %s = %s \\end{lstlisting}" fqi qt.answer );

     fprintf f "\\end{qu}\n"



let copy_content f fn =
  let fi = open_in fn in
  try while true do
        output_string f ((input_line fi)^"\n")
      done with
    End_of_file -> ();
                   close_in fi

let gen_tex ?(corrige=false) ?number f name bank =
  copy_content f "header.tex";
  fprintf f "\\rhead{Sujet : \\textbf{%s}}
\\lfoot{QB:%s}
\\headheight 35pt

\\begin{document}
\\subjectheader{} " name (Digest.to_hex bank.digest);
  let n = match number with
      Some x -> min x (Array.length bank.questions)
    | None -> Array.length bank.questions in
  for i =0 to n -1 do
    gen_question ~corrige f (i+1) bank.questions.(i)
  done;
  copy_content f "footer.tex"


let gen_question_corrige f id = function
    Value qu -> Printf.fprintf f "let fq_%i = %s;;\n" id qu.answer
  | Type qu -> Printf.fprintf f "type tq_%i = %s;;\n" id qu.answer

let gen_corrige login ?number qbank =
  let f = open_out (login^".ml") in
  Printf.fprintf f "(* QB: %s\n  Login:%s*)\n" (Digest.to_hex qbank.digest) login;
  let n = match number with
      Some x -> min x (Array.length qbank.questions)
    | None -> Array.length qbank.questions in
  for i =0 to n -1 do
    gen_question_corrige f (i+1) qbank.questions.(i)
  done;
  close_out f;

(*load the type librairie *)
open Type_lib

let list_student = [|
    "21514783"; "21711837"; "21717308"; "21719905"; "21803481"; "21807058"; "21923103"; "21934335"; "21995382"; "21999641"; "u21312490"; "u21602300"; "U21603413"; "u21714893"; "u21716236"; "u21717984"; "u21717984"; "u21719034"; "u21719337"; "u21719485"; "u21725176"; "u21800080"; "u21801408"; "u21802708"; "u21803070"; "u21805686"; "u21806498"; "u21807694"; "u21808304"; "u21808537"; "u21808662"; "u21810118"; "u21810255"; "u21810787"; "U21811297"; "u21811506"; "u21811960"; "u21812100"; "u21812324"; "u21812906"; "u21812973"; "u21813036"; "u21813233"; "u21813636"; "u21813636"; "u21813851"; "u21813989"; "U21813989"; "U21814726"; "u21815749"; "u21815966"; "u21816059"; "u21816060"; "u21816216"; "u21816239"; "u21816283"; "u21816582"; "u21816998"; "u21817310"; "u21817761"; "u21818547"; "u21913593"; "u21914655"; "u21914854 "; "u21932916"; "u21952218"; "u21954458"; "u21955218"; "u21970138"; "u21973918"; "u21980438"; "u21983521"; "u21984346"; "u21987540"; "u21988083"; "u21993512"; "u21993992"; "u21996198"; "u21997358"; "U31900295"; "u31900346"; "U31901036"; "u31901111"; "u31901426"; "U3190177"; "u31901929"; "u31902222"; "u31902375"; "u32003126"; "u32016465"; "u32018473"; "u32022941" |]


(*"u21719905"; "u21811440"; "u21805279"; "u21812378"; "u21701883"; "u21812320"; "u21506167"; "u21716108"; "u21811479"; "u21816146"; "u21814172"; "u21805265"; "u21718794"; "u21711837"; "u21807058"; "u21810255"; "u21812357"; "u21913918"; "u21816582"; "u21815359"; "u21807043"; "u21816598"; "u21813667"; "u21816610"; "u21813195"; "u21718354"; "u21709943"; "u21816112"; "u21715873"; "u21812318"; "u21816628"; "u21805267"; "u21817609"; "u21807652"; "u21809915"; "u21802708"; "u21806498"; "u21715994"; "u21717989"; "u21723813"; "u21702785"; "u21803202"; "u21811506"; "u21809281"; "u21603303"; "u21811778"; "u21312490"; "u21603445"; "u21810824"; "u21816591"; "u21812888"; "u21813418"; "u21811788"; "u21806048"; "u21719327"; "u21514783"; "u21808311"; "u31907166"; "mas-vincent"; "u21822323"; "u21817319"; "u21816363"; "u21719945"; "u21817629"; "u21816623"; "u21603033"; "u21809480"; "u21717308"; "u21713085"; "u21814219"; "u21961198"; "u21800584"; "u21802437"; "u21966878"; "u21960898"; "u21987348"; "u21950678"; "u21914233"; "u21934335"; "u21911351"; "u21987945"; "u21621719"; "u31902708"; "u21816998"; "u21813979"; "u21914153"; "u21927215"; "u21812096"; "u31903294"; "u21815693"; "u21802861"; "u21815764" |]*)




let qbankname = ref ""
let login = ref "bpicsou"
let is_corrige = ref false
let number_of_question = ref None
let _ =
  let narg = ref 0 in
  Arg.parse ["-n", Arg.Int (fun i -> number_of_question:= Some i) , "Number of question";
             "-c", Arg.Set is_corrige, "print correction"]
            (fun s -> incr narg; match !narg with
                                   1 -> qbankname := s
                                 | 2 -> login := s
                                 | _ -> raise @@ Arg.Bad "Too many arguments") "./gen_test_subject [-n n] [-c] qbank"


let gen_for_login login =
  let f = open_out (login^".tex") in
  let qbank = load_qbank !qbankname in
  let qbank2 = shuffle login qbank in
  gen_tex ~corrige:!is_corrige ?number:!number_of_question f login qbank2;
  close_out f;
  gen_corrige ?number:!number_of_question login qbank2;
  ignore (Sys.command ("pdflatex "^(login^".tex")))


let _ =
  gen_for_login !login


(*let _ =
  Array.iter gen_for_login list_student
 *)
(*let gen_all =
  gen_for_login !login
 *)

(*let _ = corrige_fq ();;
#use "reponse.ml"
 *)
