(***************************************************************************)
(*                                 Morsmall                                *)
(*                      A concise AST for POSIX shell                      *)
(*                                                                         *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Ralf Treinen,          *)
(*  Nicolas Jeannerod                                                      *)
(*                                                                         *)
(*  This program is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation, either version 3 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *)
(***************************************************************************)

open Morbig.CST

(* Helpers about locations. *)

let convert_location : 'a 'b. ('a -> 'b) -> 'a located -> 'b AST.located =
  fun f loc -> { value = f loc.value ; position = loc.position }

let convert_location_2 : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a located -> 'b -> 'c AST.located =
  fun f loc x ->
  { value = f loc.value x ; position = loc.position }

let erase_location : 'a 'b. ('a -> 'b) -> 'a located -> 'b =
  fun f x -> f x.value

(* Convertion functions *)

(* CST.program -> AST.program *)

let rec program__to__program = function
  | Program_LineBreak_CompleteCommands_LineBreak (_, complete_commands', _) ->
     complete_commands'__to__command'_list complete_commands'
  | Program_LineBreak _ ->
     []

and program'__to__program (program' : program') : AST.program =
  erase_location program__to__program program'

(* CST.complete_commands -> AST.command list *)

and complete_commands__to__command_list = function
  | CompleteCommands_CompleteCommands_NewlineList_CompleteCommand (complete_commands', _, complete_command') ->
     (complete_commands'__to__command'_list complete_commands')
     @ [complete_command'__to__command' complete_command']
  | CompleteCommands_CompleteCommand complete_command' ->
     [complete_command'__to__command' complete_command']

and complete_commands'__to__command'_list (complete_commands' : complete_commands') : AST.command' list =
  erase_location complete_commands__to__command_list complete_commands'

(* CST.complete_command -> AST.command option *)

and complete_command__to__command = function
  | CompleteCommand_CList_SeparatorOp (clist', sepop') ->
     clist'__to__command clist'
     |> separator_op'__to__command sepop'
  | CompleteCommand_CList clist' ->
     clist'__to__command clist'

and complete_command'__to__command' (complete_command' : complete_command') : AST.command' =
  convert_location complete_command__to__command complete_command'

(* CST.clist -> AST.command *)

and clist__to__command : clist -> AST.command = function
  | CList_CList_SeparatorOp_AndOr (clist', sep_op', and_or') ->
     AST.Seq (
         clist'__to__command' clist'
         |> separator_op'__to__command' sep_op',
         and_or'__to__command' and_or'
       )
  | CList_AndOr and_or' ->
     and_or'__to__command and_or'

and clist'__to__command (clist' : clist') : AST.command =
  erase_location clist__to__command clist'

and clist'__to__command' (clist' : clist') : AST.command' =
  convert_location clist__to__command clist'

(* CST.and_or -> AST.command *)

and and_or__to__command : and_or -> AST.command = function
  | AndOr_Pipeline pipeline' ->
     pipeline'__to__command pipeline'
  | AndOr_AndOr_AndIf_LineBreak_Pipeline (and_or', _, pipeline') ->
     AST.And (
         and_or'__to__command' and_or',
         pipeline'__to__command' pipeline'
       )
  | AndOr_AndOr_OrIf_LineBreak_Pipeline (and_or', _, pipeline') ->
     AST.Or (
         and_or'__to__command' and_or',
         pipeline'__to__command' pipeline'
       )

and and_or'__to__command (and_or' : and_or') : AST.command =
  erase_location and_or__to__command and_or'

and and_or'__to__command' (and_or' : and_or') : AST.command' =
  convert_location and_or__to__command and_or'

(* CST.pipeline -> AST.command *)

and pipeline__to__command : pipeline -> AST.command = function
  | Pipeline_PipeSequence pipe_sequence' ->
     pipe_sequence'__to__command pipe_sequence'
  | Pipeline_Bang_PipeSequence pipe_sequence' ->
     AST.Not (pipe_sequence'__to__command' pipe_sequence')

and pipeline'__to__command (pipeline' : pipeline') : AST.command =
  erase_location pipeline__to__command pipeline'

and pipeline'__to__command' (pipeline' : pipeline') : AST.command' =
  convert_location pipeline__to__command pipeline'

(* CST.pipe_sequence -> AST.command *)

and pipe_sequence__to__command : pipe_sequence -> AST.command = function
  | PipeSequence_Command command' ->
     command'__to__command command'
  | PipeSequence_PipeSequence_Pipe_LineBreak_Command (pipe_sequence', _, command') ->
     AST.Pipe (
         pipe_sequence'__to__command' pipe_sequence',
         command'__to__command' command'
       )

and pipe_sequence'__to__command (pipe_sequence') : AST.command =
  erase_location pipe_sequence__to__command pipe_sequence'

and pipe_sequence'__to__command' (pipe_sequence') : AST.command' =
  convert_location pipe_sequence__to__command pipe_sequence'

(* CST.command -> AST.command *)

and command__to__command : command -> AST.command = function
  | Command_SimpleCommand simple_command' ->
     simple_command'__to__command simple_command'
  | Command_CompoundCommand compound_command' ->
     compound_command'__to__command compound_command'
  | Command_CompoundCommand_RedirectList (compound_command', redirect_list') ->
     compound_command'__to__command' compound_command'
     |> redirect_list'__to__command redirect_list'
  | Command_FunctionDefinition function_definition' ->
     function_definition'__to__command function_definition'

and command'__to__command (command' : command') : AST.command =
  erase_location command__to__command command'

and command'__to__command' (command' : command') : AST.command' =
  convert_location command__to__command command'

(* CST.compound_command -> AST.command *)

and compound_command__to__command : compound_command -> AST.command = function
  | CompoundCommand_BraceGroup brace_group' ->
     brace_group'__to__command brace_group'
  | CompoundCommand_Subshell subshell' ->
     subshell'__to__command subshell'
  | CompoundCommand_ForClause for_clause' ->
     for_clause'__to__command for_clause'
  | CompoundCommand_CaseClause case_clause' ->
     case_clause'__to__command case_clause'
  | CompoundCommand_IfClause if_clause' ->
     if_clause'__to__command if_clause'
  | CompoundCommand_WhileClause while_clause' ->
     while_clause'__to__command while_clause'
  | CompoundCommand_UntilClause until_clause' ->
     until_clause'__to__command until_clause'

and compound_command'__to__command (compound_command' : compound_command') : AST.command =
  erase_location compound_command__to__command compound_command'

and compound_command'__to__command' (compound_command' : compound_command') : AST.command' =
  convert_location compound_command__to__command compound_command'

(* CST.subshell -> AST.command *)

and subshell__to__command : subshell -> AST.command  = function
  | Subshell_Lparen_CompoundList_Rparen compound_list' ->
     AST.Subshell (compound_list'__to__command' compound_list')

and subshell'__to__command (subshell' : subshell') : AST.command =
  erase_location subshell__to__command subshell'

(* CST.compound_list -> AST.command *)

and compound_list__to__command : compound_list -> AST.command = function
  | CompoundList_LineBreak_Term (_, term') ->
     term'__to__command term'
  | CompoundList_LineBreak_Term_Separator (_, term', sep') ->
     term'__to__command term'
     |> separator'__to__command sep'

and compound_list'__to__command (compound_list' : compound_list') : AST.command =
  erase_location compound_list__to__command compound_list'

and compound_list'__to__command' (compound_list' : compound_list') : AST.command' =
  convert_location compound_list__to__command compound_list'

(* CST.term -> AST.command *)

and term__to__command : term -> AST.command = function
  | Term_Term_Separator_AndOr (term', sep', and_or') ->
     AST.Seq (
         term'__to__command' term'
         |> separator'__to__command' sep',
         and_or'__to__command' and_or'
       )
  | Term_AndOr and_or' ->
     and_or'__to__command and_or'

and term'__to__command (term' : term') : AST.command =
  erase_location term__to__command term'

and term'__to__command' (term' : term') : AST.command' =
  convert_location term__to__command term'

(* CST.for_clause -> AST.command *)

and for_clause__to__command : for_clause -> AST.command = function
  | ForClause_For_Name_DoGroup (name', do_group')
  | ForClause_For_Name_SequentialSep_DoGroup (name', _, do_group') ->
     AST.For (
         name'__to__name name' ,
         None ,
         do_group'__to__command' do_group'
       )
  | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (name', _, _, do_group') ->
     AST.For (
         name'__to__name name' ,
         Some [] ,
         do_group'__to__command' do_group'
     )
  | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (name', _, wordlist', _, do_group') ->
     AST.For (
         name'__to__name name' ,
         Some (wordlist'__to__word_list wordlist') ,
         do_group'__to__command' do_group'
       )

and for_clause'__to__command (for_clause' : for_clause') : AST.command =
  erase_location for_clause__to__command for_clause'

(* CST.wordlist -> AST.word list *)

and wordlist__to__word_list : wordlist -> AST.word list = function (*FIXME*)
  | WordList_WordList_Word (wordlist', word') ->
     (wordlist'__to__word_list wordlist')
     @ [word'__to__word word']
  | WordList_Word word' ->
     [word'__to__word word']

and wordlist'__to__word_list (wordlist' : wordlist') : AST.word list =
  erase_location wordlist__to__word_list wordlist'

(* CST.case_clause -> AST.command *)

and case_clause__to__command : case_clause -> AST.command = function
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (word', _, _, case_list') ->
     AST.Case (
         word'__to__word word' ,
         case_list'__to__case_item'_list case_list'
       )
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (word', _, _, case_list_ns') ->
     AST.Case (
         word'__to__word word' ,
         case_list_ns'__to__case_item'_list case_list_ns'
       )
  | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (word', _, _) ->
     AST.Case (
         word'__to__word word' ,
         []
       )

and case_clause'__to__command (case_clause' : case_clause') : AST.command =
  erase_location case_clause__to__command case_clause'

(* CST.case_list_ns -> AST.case list *)

and case_list_ns__to__case_item'_list : case_list_ns -> AST.case_item' list = function (*FIXME*)
  | CaseListNS_CaseList_CaseItemNS (case_list', case_item_ns') ->
     (case_list'__to__case_item'_list case_list')
     @ [case_item_ns'__to__case_item' case_item_ns']
  | CaseListNS_CaseItemNS case_item_ns' ->
     [case_item_ns'__to__case_item' case_item_ns']

and case_list_ns'__to__case_item'_list (case_list_ns' : case_list_ns') : AST.case_item' list =
  erase_location case_list_ns__to__case_item'_list case_list_ns'

(* CST.case_list -> AST.case list *)

and case_list__to__case_item'_list : case_list -> AST.case_item' list = function (*FIXME*)
  | CaseList_CaseList_CaseItem (case_list', case_item') ->
     (case_list'__to__case_item'_list case_list')
     @ [case_item'__to__case_item' case_item']
  | CaseList_CaseItem case_item' ->
     [case_item'__to__case_item' case_item']

and case_list'__to__case_item'_list (case_list' : case_list') : AST.case_item' list =
  erase_location case_list__to__case_item'_list case_list'

(* CST.case_item_ns -> AST.case_item *)

and case_item_ns__to__case_item : case_item_ns -> AST.case_item = function
  | CaseItemNS_Pattern_Rparen_LineBreak (pattern', _)
  | CaseItemNS_Lparen_Pattern_Rparen_LineBreak (pattern', _) ->
     (pattern'__to__pattern' pattern', None)
  | CaseItemNS_Pattern_Rparen_CompoundList (pattern', compound_list')
  | CaseItemNS_Lparen_Pattern_Rparen_CompoundList (pattern', compound_list') ->
     (pattern'__to__pattern' pattern', Some (compound_list'__to__command' compound_list'))

and case_item_ns'__to__case_item' (case_item_ns' : case_item_ns') : AST.case_item' =
  convert_location case_item_ns__to__case_item case_item_ns'

(* CST.case_item -> AST.case_item *)

and case_item__to__case_item : case_item -> AST.case_item = function
  | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (pattern', _, _)
  | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (pattern', _, _) ->
     (pattern'__to__pattern' pattern', None)
  | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (pattern', compound_list', _)
  | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (pattern', compound_list', _) ->
     (pattern'__to__pattern' pattern', Some (compound_list'__to__command' compound_list'))

and case_item'__to__case_item' (case_item' : case_item') : AST.case_item' =
  convert_location case_item__to__case_item case_item'

(* CST.pattern -> AST.pattern *)

and pattern__to__pattern : pattern -> AST.pattern = function
  | Pattern_Word word' ->
     [word'__to__word word']
  | Pattern_Pattern_Pipe_Word (pattern', word') ->
     (pattern'__to__pattern pattern')
     @ [word'__to__word word']

and pattern'__to__pattern (pattern' : pattern') : AST.pattern =
  erase_location pattern__to__pattern pattern'

and pattern'__to__pattern' (pattern' : pattern') : AST.pattern' =
  convert_location pattern__to__pattern pattern'

(* CST.if_clause -> AST.command *)

and if_clause__to__command : if_clause -> AST.command = function
  | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (compound_list', compound_list2', else_part') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         Some (else_part'__to__command' else_part')
       )
  | IfClause_If_CompoundList_Then_CompoundList_Fi (compound_list', compound_list2') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         None
       )

and if_clause'__to__command (if_clause' : if_clause') : AST.command =
  erase_location if_clause__to__command if_clause'

(* CST.else_part -> AST.command *)

and else_part__to__command : else_part -> AST.command = function
  | ElsePart_Elif_CompoundList_Then_CompoundList (compound_list', compound_list2') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         None
       )
  | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (compound_list', compound_list2', else_part') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         Some (else_part'__to__command' else_part')
       )
  | ElsePart_Else_CompoundList compound_list' ->
     compound_list'__to__command compound_list'

and else_part'__to__command' (else_part' : else_part') : AST.command' =
  convert_location else_part__to__command else_part'

(* CST.while_clause -> AST.command *)

and while_clause__to__command : while_clause -> AST.command = function
  | WhileClause_While_CompoundList_DoGroup (compound_list', do_group') ->
     AST.While (
         compound_list'__to__command' compound_list' ,
         do_group'__to__command' do_group'
       )

and while_clause'__to__command (while_clause' : while_clause') : AST.command =
  erase_location while_clause__to__command while_clause'

(* CST.until_clause -> AST.command *)

and until_clause__to__command : until_clause -> AST.command = function
  | UntilClause_Until_CompoundList_DoGroup (compound_list', do_group') ->
     AST.Until (
         compound_list'__to__command' compound_list' ,
         do_group'__to__command' do_group'
       )

and until_clause'__to__command (until_clause' : until_clause') : AST.command =
  erase_location until_clause__to__command until_clause'

(* CST.function_definition -> AST.command *)

and function_definition__to__command : function_definition -> AST.command = function
  | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody (fname', _, function_body') ->
     AST.Function (
         fname'__to__name fname' ,
         function_body'__to__command' function_body'
       )

and function_definition'__to__command (function_definition' : function_definition') : AST.command =
  erase_location function_definition__to__command function_definition'

(* CST.function_body -> AST.command *)

and function_body__to__command : function_body -> AST.command = function
  | FunctionBody_CompoundCommand compound_command' ->
     compound_command'__to__command compound_command'
  | FunctionBody_CompoundCommand_RedirectList (compound_command', redirect_list') ->
     compound_command'__to__command' compound_command'
     |> redirect_list'__to__command redirect_list'

and function_body'__to__command' (function_body' : function_body') : AST.command' =
  convert_location function_body__to__command function_body'

(* CST.fname -> AST.name *)

and fname__to__name : fname -> AST.name = function
  | Fname_Name name -> name__to__name name

and fname'__to__name (fname' : fname') : AST.name =
  erase_location fname__to__name fname'

(* CST.brace_group -> AST.command *)

and brace_group__to__command : brace_group -> AST.command = function
  | BraceGroup_LBrace_CompoundList_RBrace compound_list' ->
     compound_list'__to__command compound_list'

and brace_group'__to__command (brace_group' : brace_group') : AST.command =
  erase_location brace_group__to__command brace_group'

(* CST.do_group -> AST.command *)

and do_group__to__command : do_group -> AST.command = function
  | DoGroup_Do_CompoundList_Done compound_list' ->
     compound_list'__to__command compound_list'

and do_group'__to__command' (do_group' : do_group') : AST.command' =
  convert_location do_group__to__command do_group'

(* CST.simple_command -> AST.command *)

and simple_command'__to__command (simple_command' : simple_command') : AST.command =
  let ( assignment'_list , word'_list , io_redirect'_list ) =
    match simple_command'.value with
    | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', cmd_word', cmd_suffix') ->
       (* Since we are sorting right-to-left, we need to sort the
          suffix before the prefix. *)
       let ( word'_list , io_redirect'_list ) = sort__cmd_suffix' [] [] cmd_suffix' in
       let ( assignment_word'_list , io_redirect'_list ) = sort__cmd_prefix' [] io_redirect'_list cmd_prefix' in
       (
         List.map assignment_word'__to__assignment' assignment_word'_list ,
         cmd_word'__to__word' cmd_word' :: List.map word'__to__word' word'_list ,
         io_redirect'_list
       )

    | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', cmd_word') ->
       let ( assignment_word'_list , io_redirect'_list ) = sort__cmd_prefix' [] [] cmd_prefix' in
       (
         List.map assignment_word'__to__assignment' assignment_word'_list ,
         cmd_word'__to__word' cmd_word' :: [] ,
         io_redirect'_list
       )

    | SimpleCommand_CmdPrefix cmd_prefix' ->
       let ( assignment_word'_list , io_redirect'_list ) = sort__cmd_prefix' [] [] cmd_prefix' in
       ( List.map assignment_word'__to__assignment' assignment_word'_list ,
         [] ,
         io_redirect'_list )

    | SimpleCommand_CmdName_CmdSuffix (cmd_name', cmd_suffix') ->
       let ( word'_list , io_redirect'_list ) = sort__cmd_suffix' [] [] cmd_suffix' in
       ( [] ,
         cmd_name'__to__word' cmd_name' :: List.map word'__to__word' word'_list ,
         io_redirect'_list )

    | SimpleCommand_CmdName cmd_name' ->
       ( [] ,
         cmd_name'__to__word' cmd_name' :: [] ,
         [] )
  in
  (* Because of the semantics of redirections, we need to handle that
     redirection list from right to left. *)
  List.fold_right
    (
      fun io_redirect' command ->
      io_redirect'__to__command
        io_redirect'
        { value = command ;
          position = simple_command'.position }
    )
    io_redirect'_list
    (AST.Simple (assignment'_list, word'_list ))

(* CST.cmd_prefix -> CST.assignment_word' list * CST.io_redirect' list

   This function takes a prefix (which is basically a list of either
   CST.assignment_word' or CST.io_redirect' and return two separate
   lists for these two type of elements. It uses accumulators, but
   since we are converting right-to-left lists to left-to-right lists,
   we do not need a List.rev. *)

and sort__cmd_prefix (assignment_word'_acc : assignment_word' list) (io_redirect'_acc : io_redirect' list) (*FIXME: check order*)
    : cmd_prefix -> assignment_word' list * io_redirect' list = function
  | CmdPrefix_IoRedirect io_redirect' ->
     ( assignment_word'_acc ,
       io_redirect' :: io_redirect'_acc )

  | CmdPrefix_CmdPrefix_IoRedirect (cmd_prefix', io_redirect') ->
     sort__cmd_prefix'
       assignment_word'_acc
       (io_redirect' :: io_redirect'_acc)
       cmd_prefix'

  | CmdPrefix_AssignmentWord assignment_word' ->
     ( assignment_word' :: assignment_word'_acc ,
       io_redirect'_acc )

  | CmdPrefix_CmdPrefix_AssignmentWord (cmd_prefix', assignment_word') ->
     sort__cmd_prefix'
       (assignment_word' :: assignment_word'_acc)
       io_redirect'_acc
       cmd_prefix'

and sort__cmd_prefix' (assignment_word'_acc : assignment_word' list) (io_redirect'_acc : io_redirect' list)
    (cmd_prefix' : cmd_prefix') : assignment_word' list * io_redirect' list =
  sort__cmd_prefix assignment_word'_acc io_redirect'_acc cmd_prefix'.value

(* CST.cmd_suffix -> CST.word' list * CST.io_redirect' list

   This function takes a suffix (which is basically a list of either
   CST.word' or CST.io_redirect' and return two separate lists for
   these two type of elements. It uses accumulators, but since we are
   converting right-to-left lists to left-to-right lists, we do not
   need a List.rev. *)

and sort__cmd_suffix (word'_acc : word' list) (io_redirect'_acc : io_redirect' list)
    : cmd_suffix -> word' list * io_redirect' list = function
  | CmdSuffix_IoRedirect io_redirect' ->
     ( word'_acc ,
       io_redirect' :: io_redirect'_acc )

  | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix', io_redirect') ->
     sort__cmd_suffix'
       word'_acc
       (io_redirect' :: io_redirect'_acc)
       cmd_suffix'

  | CmdSuffix_Word word' ->
     ( word' :: word'_acc ,
       io_redirect'_acc )

  | CmdSuffix_CmdSuffix_Word (cmd_suffix', word') ->
     sort__cmd_suffix'
       (word' :: word'_acc)
       io_redirect'_acc
       cmd_suffix'

and sort__cmd_suffix' (word'_acc : word' list) (io_redirect'_acc : io_redirect' list)
    (cmd_suffix' : cmd_suffix') : word' list * io_redirect' list =
  sort__cmd_suffix word'_acc io_redirect'_acc cmd_suffix'.value

(* CST.cmd_name -> AST.word *)

and cmd_name__to__word : cmd_name -> AST.word = function
  | CmdName_Word word' ->
     word'__to__word word'

and cmd_name'__to__word' (cmd_name' : cmd_name') : AST.word' =
  convert_location cmd_name__to__word cmd_name'

(* CST.cmd_word -> AST.word *)

and cmd_word__to__word : cmd_word -> AST.word = function
  | CmdWord_Word word' ->
     word'__to__word word'

and cmd_word'__to__word' (cmd_word' : cmd_word') : AST.word' =
  convert_location cmd_word__to__word cmd_word'

(* CST.redirect_list -> AST.command' -> AST.command *)

and redirect_list__to__command redirect_list (command' : AST.command') : AST.command =
  match redirect_list with
  | RedirectList_IoRedirect io_redirect' ->
     command'
     |> io_redirect'__to__command io_redirect'
  | RedirectList_RedirectList_IoRedirect (redirect_list', io_redirect') ->
     command'
     |> io_redirect'__to__command' io_redirect'
     |> redirect_list'__to__command redirect_list' (*FIXME: check order of the redirections*)

and redirect_list'__to__command (redirect_list' : redirect_list') (command' : AST.command') : AST.command =
  erase_location redirect_list__to__command redirect_list' command'

(* CST.io_redirect -> AST.command' -> AST.command *)

and io_redirect__to__command (io_redirect : io_redirect) (command' : AST.command') : AST.command =
  match io_redirect with
  | IoRedirect_IoFile io_file' ->
     let kind, word = io_file'__to__kind_word io_file' in
     AST.Redirection (
         command' ,
         (AST.default_redirection_descriptor kind) ,
         kind ,
         word
       )
  | IoRedirect_IoNumber_IoFile (io_number, io_file') ->
     let kind, word = io_file'__to__kind_word io_file' in
     AST.Redirection (
         command' ,
         (io_number__to__int io_number) ,
         kind ,
         word
       )
  | IoRedirect_IoHere io_here' ->
     let _strip, word' = io_here'__to__strip_word' io_here' in
     AST.HereDocument (
         command' ,
         0 ,
         word' (* FIXME: strip that word if needed *)
       )
  | IoRedirect_IoNumber_IoHere (io_number, io_here') ->
     let _strip, word' = io_here'__to__strip_word' io_here' in
     AST.HereDocument (
         command' ,
         (io_number__to__int io_number) ,
         word' (* FIXME: strip that word if needed *)
       )

and io_redirect'__to__command (io_redirect' : io_redirect') (command' : AST.command') : AST.command =
  erase_location io_redirect__to__command io_redirect' command'

and io_redirect'__to__command' (io_redirect' : io_redirect') (command' : AST.command') : AST.command' =
  convert_location_2 io_redirect__to__command io_redirect' command'

(* CST.io_file -> AST.redirection_kind * AST.word *)

and io_file__to__kind_word io_file =
  let kind, filename' =
    match io_file with
    | IoFile_Less_FileName filename' -> AST.Input, filename'
    | IoFile_LessAnd_FileName filename' -> AST.InputDuplicate, filename'
    | IoFile_Great_FileName filename' -> AST.Output, filename'
    | IoFile_GreatAnd_FileName filename' -> AST.OutputDuplicate, filename'
    | IoFile_DGreat_FileName filename' -> AST.OutputAppend, filename'
    | IoFile_LessGreat_FileName filename' -> AST.InputOutput, filename'
    | IoFile_Clobber_FileName filename' -> AST.OutputClobber, filename'
  in
  ( kind , filename'__to__word filename' )

and io_file'__to__kind_word (io_file' : io_file') : AST.kind * AST.word =
  erase_location io_file__to__kind_word io_file'

(* CST.filename -> AST.word *)

and filename__to__word : filename -> AST.word = function
  | Filename_Word word' ->
     word'__to__word word'

and filename'__to__word (filename' : filename') : AST.word =
  erase_location filename__to__word filename'

(* CST.io_here -> bool * AST.word *)

and io_here__to__strip_word' : io_here -> bool * AST.word' = function
  | IoHere_DLess_HereEnd (_, word'_ref) ->
     (false, word'__to__word' !word'_ref)
  | IoHere_DLessDash_HereEnd (_, word'_ref) ->
     (true, word'__to__word' !word'_ref)

and io_here'__to__strip_word' (io_here' : io_here') : bool * AST.word' =
  erase_location io_here__to__strip_word' io_here'

(* CST.separator_op -> AST.command -> AST.command *)

and separator_op__to__command (sep_op : separator_op) (command : AST.command) : AST.command =
  match sep_op with
  | SeparatorOp_Uppersand -> AST.Async command
  | SeparatorOp_Semicolon -> command

and separator_op'__to__command (sep_op' : separator_op') (command : AST.command) : AST.command =
  erase_location separator_op__to__command sep_op' command

and separator_op'__to__command' (sep_op' : separator_op') (command' : AST.command') : AST.command' =
  (* We do not want to convert the separator's location here but
     rather use the command's location! *)
  { value = separator_op__to__command sep_op'.value command'.value ;
    position = command'.position }

(* CST.separator -> AST.command -> AST.command *)

and separator__to__command (sep : separator) (command : AST.command) : AST.command =
  match sep with
  | Separator_SeparatorOp_LineBreak (sep_op', _) ->
     separator_op'__to__command sep_op' command
  | Separator_NewLineList _ ->
     command

and separator'__to__command (sep' : separator') (command : AST.command) : AST.command =
  erase_location separator__to__command sep' command

and separator'__to__command' (sep' : separator') (command' : AST.command') : AST.command' =
  (* We do not want to convert the separator's location here but
     rather use the command's location! *)
  { value = separator__to__command sep'.value command'.value ;
    position = command'.position }

(* *)

and sequential_sep__to__command _ (command : AST.command) : AST.command =
  command

(* CST.word -> AST.word *)

and word__to__word : word -> AST.word = function
  | Word (_, word_cst) ->
     word_cst__to__word word_cst

and word'__to__word (word' : word') : AST.word =
  erase_location word__to__word word'

and word'__to__word' (word' : word') : AST.word' =
  convert_location word__to__word word'

and word_double_quoted__to__word (Word (_, word_cst)) =
  word_cst_double_quoted__to__word word_cst

(* CST.word_cst -> AST.word *)

and word_cst__to__word (word_cst : word_cst) : AST.word =
  List.map word_component__to__word_component word_cst

and word_cst_double_quoted__to__word (word_cst : word_cst) : AST.word =
  List.map word_component_double_quoted__to__word_component word_cst

(* CST.word_component -> AST.word_component *)

and word_component__to__word_component = function
  | WordSubshell (_, program') ->
     AST.Subshell (program'__to__program program')
  | WordName name ->
     AST.Name name (* FIXME: literal? *)
  | WordAssignmentWord assignment_word ->
     AST.Assignment (assignment_word__to__assignment assignment_word)
  | WordDoubleQuoted word ->
     AST.DoubleQuoted (word_double_quoted__to__word word)
  | WordSingleQuoted (Word (_, [WordLiteral literal])) ->
     AST.Literal literal
  | WordSingleQuoted (Word (_, [])) ->
     AST.Literal ""
  | WordSingleQuoted _ ->
     assert false
  | WordLiteral literal ->
     AST.Literal literal
  | WordVariable (VariableAtom (name, variable_attribute)) ->
     AST.Variable (name, variable_attribute__to__attribute variable_attribute)
  | WordGlobAll ->
     AST.GlobAll
  | WordGlobAny ->
     AST.GlobAny
  | WordGlobRange (Range char_list) ->
     AST.GlobRange char_list
  | WordOther | WordEmpty ->
     assert false

and word_component_double_quoted__to__word_component = function
  | WordSubshell (_, program') ->
     AST.Subshell (program'__to__program program')
  | WordName name ->
     AST.Name name (* FIXME: literal? *)
  | WordAssignmentWord assignment_word ->
     AST.Assignment (assignment_word__to__assignment assignment_word)
  | WordLiteral literal ->
     AST.Literal literal
  | WordVariable (VariableAtom (name, variable_attribute)) ->
     AST.Variable (name, variable_attribute__to__attribute variable_attribute)
  | WordDoubleQuoted _ | WordSingleQuoted _
  | WordGlobAll | WordGlobAny | WordGlobRange _
  | WordOther | WordEmpty ->
     assert false

and variable_attribute__to__attribute = function
  | NoAttribute ->
     AST.NoAttribute
  | UseDefaultValues word ->
     AST.UseDefaultValues (word__to__word word)
  | AssignDefaultValues word ->
     AST.AssignDefaultValues (word__to__word word)
  | IndicateErrorifNullorUnset word ->
     AST.IndicateErrorifNullorUnset (word__to__word word)
  | UseAlternativeValue word ->
     AST.UseAlternativeValue (word__to__word word)
  | RemoveSmallestSuffixPattern word ->
     AST.RemoveSmallestSuffixPattern (word__to__word word)
  | RemoveLargestSuffixPattern word ->
     AST.RemoveLargestSuffixPattern (word__to__word word)
  | RemoveSmallestPrefixPattern word ->
     AST.RemoveSmallestPrefixPattern (word__to__word word)
  | RemoveLargestPrefixPattern word ->
     AST.RemoveLargestPrefixPattern (word__to__word word)

(* CST.name -> AST.name *)

and name__to__name : name -> AST.name = function
  | Name name -> name

and name'__to__name (name' : name') : AST.name =
  erase_location name__to__name name'

(* CST.assignment_word -> AST.assignment *)

and assignment_word__to__assignment ((name, word) : assignment_word) : AST.assignment =
  (name__to__name name,
   word__to__word word)

and assignment_word'__to__assignment' (assignment_word' : assignment_word') : AST.assignment' =
  convert_location assignment_word__to__assignment assignment_word'

(* CST.io_number -> AST.descr *)

and io_number__to__int  = function
  | IONumber io_number -> int_of_string io_number
