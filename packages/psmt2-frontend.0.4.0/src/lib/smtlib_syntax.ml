(******************************************************************************)
(*                                                                            *)
(* An SMT-LIB 2 for the Alt-Ergo Theorem Prover                               *)
(*                                                                            *)
(******************************************************************************)

type 'a data =
  { p : (Lexing.position * Lexing.position) option ; c : 'a ;
    ty : Smtlib_ty.ty; mutable is_quantif : bool}

type constant =
  | Const_Dec of string
  | Const_Num of string
  | Const_Str of string
  | Const_Hex of string
  | Const_Bin of string

type symbol = string data

type keyword_aux =
  | Category
  | Smtlibversion
  | Source
  | Statuts of symbol
  | License
  | Notes
  | Axioms
  | Definitio
  | Extensions
  | Funs
  | FunsDescript
  | Language
  | Sorts
  | SortsDescr
  | Theories
  | Values
and keyword = keyword_aux data

and key_option_aux =
  | Diagnooutputchan
  | Globaldeclarations
  | Interactive
  | Printsucces
  | Produceassertions
  | Produceassignement
  | Producemodels
  | Produceproofs
  | Produceunsatassumptions
  | Produceunsatcores
  | Randomseed
  | Regularoutputchan
  | Verbosity
  | Ressourcelimit
and key_option = key_option_aux data

and option_aux =
  | Option_key of key_option * index
  | Option_attribute of attribute
and option = option_aux data

and key_info_aux =
  | Allstats
  | Assertionstacklvl
  | Authors
  | Difficulty
  | Errorbehav
  | Incremental
  | Instance
  | Name
  | Reasonunknown
  | Series
  | Version
  | Key_info of keyword
and key_info = key_info_aux data

and key_term_aux =
  | Pattern of term list
  | Named of symbol
and key_term = key_term_aux data

(* attributes *)
and sexpr_aux =
  | SexprSpecConst of constant
  | SexprSymbol of symbol
  | SexprKeyword of keyword
  | SexprInParen of sexpr list
and sexpr = sexpr_aux data

and attribute_value_aux =
  | AttributeValSpecConst of constant
  | AttributeValSymbol of symbol
  | AttributeValSexpr of sexpr list
  | NoAttributeValue
and attribute_value = attribute_value_aux data

and attribute_aux =
  | AttributeKey of key_info
  | AttributeKeyValue of key_info * attribute_value
and attribute = attribute_aux data

(* index *)
and index_aux =
  | IndexSymbol of symbol
  | IndexNumeral of string
and index = index_aux data

(* identifiers *)
and identifier_aux =
  | IdSymbol of symbol
  | IdUnderscoreSymNum of symbol * index list
and identifier = identifier_aux data

and prop_literal_aux =
  | PropLit of symbol
  | PropLitNot of symbol
and prop_literal = prop_literal_aux data

(* sorts and polymorphism *)
and sort_aux =
  | SortIdentifier of identifier
  | SortIdMulti of identifier * sort list
and sort = sort_aux data

(* typed variable *)
and sorted_var = symbol * sort

(* qualidentifiers *)
and qualidentifier_aux =
  | QualIdentifierId of identifier
  | QualIdentifierAs of identifier * sort
and qualidentifier = qualidentifier_aux data

(* valued variable *)
and varbinding = symbol * term

(* pattern *)
and pattern_aux =
  | MatchPattern of (symbol * symbol list)
  | MatchUnderscore
and pattern = pattern_aux data

(* terms *)
and term_aux =
  | TermSpecConst of constant
  | TermQualIdentifier of qualidentifier
  | TermQualIdTerm of qualidentifier * term list
  | TermLetTerm of varbinding list * term
  | TermForAllTerm of sorted_var list * term
  | TermExistsTerm of sorted_var list * term
  | TermExclimationPt of term * key_term list
  | TermMatch of term * (pattern * term) list
and term = term_aux data

(* datatypes *)
and sort_dec = symbol * string
and selector_dec = symbol * sort
and constructor_dec = symbol * selector_dec list

(* script commands *)
type command_aux =
  | Cmd_Assert of (symbol list * term)
  | Cmd_CheckSat
  | Cmd_CheckAllSat of symbol list
  | Cmd_CheckSatAssum of prop_literal list
  | Cmd_CheckEntailment of (symbol list * term)
  | Cmd_DeclareConst of symbol * (symbol list * sort)
  | Cmd_DeclareDataType of symbol * ((symbol list) * (constructor_dec list))
  | Cmd_DeclareDataTypes of
      sort_dec list * ((symbol list) * (constructor_dec list)) list
  | Cmd_DeclareFun of symbol * (symbol list * sort list * sort)
  | Cmd_DeclareSort of symbol * string
  | Cmd_DefineFun of (symbol * symbol list * sorted_var list * sort) * term
  | Cmd_DefineFunRec of (symbol * symbol list * sorted_var list * sort) * term
  | Cmd_DefineFunsRec of
      (symbol * symbol list * sorted_var list * sort) list * term list
  | Cmd_DefineSort of symbol * symbol list * sort
  | Cmd_Echo of symbol
  | Cmd_GetAssert
  | Cmd_GetProof
  | Cmd_GetUnsatCore
  | Cmd_GetValue of term list
  | Cmd_GetAssign
  | Cmd_GetOption of keyword
  | Cmd_GetInfo of key_info
  | Cmd_GetModel
  | Cmd_GetUnsatAssumptions
  | Cmd_Reset
  | Cmd_ResetAssert
  | Cmd_SetLogic of symbol
  | Cmd_SetOption of option
  | Cmd_SetInfo of attribute
  | Cmd_Push of string
  | Cmd_Pop of string
  | Cmd_Exit
  | Cmd_Maximize of term
  | Cmd_Minimize of term

type command = command_aux data
type commands = command list

(*******************************************************************)
