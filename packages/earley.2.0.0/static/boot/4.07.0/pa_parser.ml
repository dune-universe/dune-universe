open Earley_core
open Asttypes
open Parsetree
open Longident
open Pa_ocaml_prelude
open Pa_lexing
open Helper
type action =
  | Default 
  | Normal of expression 
  | DepSeq of ((expression -> expression) * expression option * expression) 
let occur id e =
  Iter.do_local_ident := ((fun s -> if s = id then raise Exit));
  (try
     (match e with
      | Default -> ()
      | Normal e -> Iter.iter_expression e
      | DepSeq (_, e1, e2) ->
          (Iter.iter_option Iter.iter_expression e1; Iter.iter_expression e2));
     false
   with | Exit -> true)
let find_locate () =
  try
    Some
      (Exp.ident
         { txt = (Lident (Sys.getenv "LOCATE")); loc = Location.none })
  with | Not_found -> None
let mkpatt _loc (id, p) =
  match p with
  | None ->
      {
        Parsetree.ppat_desc =
          (Parsetree.Ppat_var { Asttypes.txt = id; Asttypes.loc = _loc });
        Parsetree.ppat_loc = _loc;
        Parsetree.ppat_attributes = []
      }
  | Some p ->
      {
        Parsetree.ppat_desc =
          (Parsetree.Ppat_alias
             (p, { Asttypes.txt = id; Asttypes.loc = _loc }));
        Parsetree.ppat_loc = _loc;
        Parsetree.ppat_attributes = []
      }
let rec build_action _loc occur_loc ids e =
  let e1 =
    List.fold_left
      (fun e ->
         fun ((id, x), visible) ->
           match ((find_locate ()), visible) with
           | (Some f2, true) ->
               {
                 Parsetree.pexp_desc =
                   (Parsetree.Pexp_fun
                      (Asttypes.Nolabel, None,
                        {
                          Parsetree.ppat_desc =
                            (Parsetree.Ppat_var
                               { Asttypes.txt = "str"; Asttypes.loc = _loc });
                          Parsetree.ppat_loc = _loc;
                          Parsetree.ppat_attributes = []
                        },
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_fun
                               (Asttypes.Nolabel, None,
                                 {
                                   Parsetree.ppat_desc =
                                     (Parsetree.Ppat_var
                                        {
                                          Asttypes.txt = "pos";
                                          Asttypes.loc = _loc
                                        });
                                   Parsetree.ppat_loc = _loc;
                                   Parsetree.ppat_attributes = []
                                 },
                                 {
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_fun
                                        (Asttypes.Nolabel, None,
                                          {
                                            Parsetree.ppat_desc =
                                              (Parsetree.Ppat_var
                                                 {
                                                   Asttypes.txt = "str'";
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.ppat_loc = _loc;
                                            Parsetree.ppat_attributes = []
                                          },
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_fun
                                                 (Asttypes.Nolabel, None,
                                                   {
                                                     Parsetree.ppat_desc =
                                                       (Parsetree.Ppat_var
                                                          {
                                                            Asttypes.txt =
                                                              "pos'";
                                                            Asttypes.loc =
                                                              _loc
                                                          });
                                                     Parsetree.ppat_loc =
                                                       _loc;
                                                     Parsetree.ppat_attributes
                                                       = []
                                                   },
                                                   {
                                                     Parsetree.pexp_desc =
                                                       (Parsetree.Pexp_fun
                                                          (Asttypes.Nolabel,
                                                            None,
                                                            (mkpatt _loc
                                                               (id, x)),
                                                            {
                                                              Parsetree.pexp_desc
                                                                =
                                                                (Parsetree.Pexp_let
                                                                   (Asttypes.Nonrecursive,
                                                                    [
                                                                    {
                                                                    Parsetree.pvb_pat
                                                                    =
                                                                    {
                                                                    Parsetree.ppat_desc
                                                                    =
                                                                    (Parsetree.Ppat_var
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    ("_loc_"
                                                                    ^ id);
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.ppat_loc
                                                                    = _loc;
                                                                    Parsetree.ppat_attributes
                                                                    = []
                                                                    };
                                                                    Parsetree.pvb_expr
                                                                    =
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_apply
                                                                    (f2,
                                                                    [
                                                                    (Asttypes.Nolabel,
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "str");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    });
                                                                    (Asttypes.Nolabel,
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "pos");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    });
                                                                    (Asttypes.Nolabel,
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "str'");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    });
                                                                    (Asttypes.Nolabel,
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "pos'");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    })]));
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    };
                                                                    Parsetree.pvb_attributes
                                                                    = [];
                                                                    Parsetree.pvb_loc
                                                                    = _loc
                                                                    }], e));
                                                              Parsetree.pexp_loc
                                                                = _loc;
                                                              Parsetree.pexp_attributes
                                                                = []
                                                            }));
                                                     Parsetree.pexp_loc =
                                                       _loc;
                                                     Parsetree.pexp_attributes
                                                       = []
                                                   }));
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          }));
                                   Parsetree.pexp_loc = _loc;
                                   Parsetree.pexp_attributes = []
                                 }));
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        }));
                 Parsetree.pexp_loc = _loc;
                 Parsetree.pexp_attributes = []
               }
           | _ when (id = "_") && (x = None) -> e
           | _ ->
               {
                 Parsetree.pexp_desc =
                   (Parsetree.Pexp_fun
                      (Asttypes.Nolabel, None, (mkpatt _loc (id, x)), e));
                 Parsetree.pexp_loc = _loc;
                 Parsetree.pexp_attributes = []
               }) e (List.rev ids) in
  match ((find_locate ()), occur_loc) with
  | (Some locate2, true) ->
      {
        Parsetree.pexp_desc =
          (Parsetree.Pexp_fun
             (Asttypes.Nolabel, None,
               {
                 Parsetree.ppat_desc =
                   (Parsetree.Ppat_var
                      {
                        Asttypes.txt = "__loc__start__buf";
                        Asttypes.loc = _loc
                      });
                 Parsetree.ppat_loc = _loc;
                 Parsetree.ppat_attributes = []
               },
               {
                 Parsetree.pexp_desc =
                   (Parsetree.Pexp_fun
                      (Asttypes.Nolabel, None,
                        {
                          Parsetree.ppat_desc =
                            (Parsetree.Ppat_var
                               {
                                 Asttypes.txt = "__loc__start__pos";
                                 Asttypes.loc = _loc
                               });
                          Parsetree.ppat_loc = _loc;
                          Parsetree.ppat_attributes = []
                        },
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_fun
                               (Asttypes.Nolabel, None,
                                 {
                                   Parsetree.ppat_desc =
                                     (Parsetree.Ppat_var
                                        {
                                          Asttypes.txt = "__loc__end__buf";
                                          Asttypes.loc = _loc
                                        });
                                   Parsetree.ppat_loc = _loc;
                                   Parsetree.ppat_attributes = []
                                 },
                                 {
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_fun
                                        (Asttypes.Nolabel, None,
                                          {
                                            Parsetree.ppat_desc =
                                              (Parsetree.Ppat_var
                                                 {
                                                   Asttypes.txt =
                                                     "__loc__end__pos";
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.ppat_loc = _loc;
                                            Parsetree.ppat_attributes = []
                                          },
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_let
                                                 (Asttypes.Nonrecursive,
                                                   [{
                                                      Parsetree.pvb_pat =
                                                        {
                                                          Parsetree.ppat_desc
                                                            =
                                                            (Parsetree.Ppat_var
                                                               {
                                                                 Asttypes.txt
                                                                   = "_loc";
                                                                 Asttypes.loc
                                                                   = _loc
                                                               });
                                                          Parsetree.ppat_loc
                                                            = _loc;
                                                          Parsetree.ppat_attributes
                                                            = []
                                                        };
                                                      Parsetree.pvb_expr =
                                                        {
                                                          Parsetree.pexp_desc
                                                            =
                                                            (Parsetree.Pexp_apply
                                                               (locate2,
                                                                 [(Asttypes.Nolabel,
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "__loc__start__buf");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    });
                                                                 (Asttypes.Nolabel,
                                                                   {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "__loc__start__pos");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                   });
                                                                 (Asttypes.Nolabel,
                                                                   {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "__loc__end__buf");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                   });
                                                                 (Asttypes.Nolabel,
                                                                   {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "__loc__end__pos");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                   })]));
                                                          Parsetree.pexp_loc
                                                            = _loc;
                                                          Parsetree.pexp_attributes
                                                            = []
                                                        };
                                                      Parsetree.pvb_attributes
                                                        = [];
                                                      Parsetree.pvb_loc =
                                                        _loc
                                                    }], e1));
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          }));
                                   Parsetree.pexp_loc = _loc;
                                   Parsetree.pexp_attributes = []
                                 }));
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        }));
                 Parsetree.pexp_loc = _loc;
                 Parsetree.pexp_attributes = []
               }));
        Parsetree.pexp_loc = _loc;
        Parsetree.pexp_attributes = []
      }
  | _ -> e1
let apply_option _loc opt e =
  let fn e f d =
    match d with
    | None ->
        {
          Parsetree.pexp_desc =
            (Parsetree.Pexp_apply
               ({
                  Parsetree.pexp_desc =
                    (Parsetree.Pexp_ident
                       {
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Ldot
                                  ((Longident.Lident "Earley_core"),
                                    "Earley")), f));
                         Asttypes.loc = _loc
                       });
                  Parsetree.pexp_loc = _loc;
                  Parsetree.pexp_attributes = []
                },
                 [(Asttypes.Nolabel,
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_construct
                           ({
                              Asttypes.txt = (Longident.Lident "None");
                              Asttypes.loc = _loc
                            }, None));
                      Parsetree.pexp_loc = _loc;
                      Parsetree.pexp_attributes = []
                    });
                 (Asttypes.Nolabel,
                   {
                     Parsetree.pexp_desc =
                       (Parsetree.Pexp_apply
                          ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_ident
                                  {
                                    Asttypes.txt =
                                      (Longident.Ldot
                                         ((Longident.Ldot
                                             ((Longident.Lident "Earley_core"),
                                               "Earley")), "apply"));
                                    Asttypes.loc = _loc
                                  });
                             Parsetree.pexp_loc = _loc;
                             Parsetree.pexp_attributes = []
                           },
                            [(Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_fun
                                      (Asttypes.Nolabel, None,
                                        {
                                          Parsetree.ppat_desc =
                                            (Parsetree.Ppat_var
                                               {
                                                 Asttypes.txt = "x";
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.ppat_loc = _loc;
                                          Parsetree.ppat_attributes = []
                                        },
                                        {
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_construct
                                               ({
                                                  Asttypes.txt =
                                                    (Longident.Lident "Some");
                                                  Asttypes.loc = _loc
                                                },
                                                 (Some
                                                    {
                                                      Parsetree.pexp_desc =
                                                        (Parsetree.Pexp_ident
                                                           {
                                                             Asttypes.txt =
                                                               (Longident.Lident
                                                                  "x");
                                                             Asttypes.loc =
                                                               _loc
                                                           });
                                                      Parsetree.pexp_loc =
                                                        _loc;
                                                      Parsetree.pexp_attributes
                                                        = []
                                                    })));
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        }));
                                 Parsetree.pexp_loc = _loc;
                                 Parsetree.pexp_attributes = []
                               });
                            (Asttypes.Nolabel, e)]));
                     Parsetree.pexp_loc = _loc;
                     Parsetree.pexp_attributes = []
                   })]));
          Parsetree.pexp_loc = _loc;
          Parsetree.pexp_attributes = []
        }
    | Some d ->
        {
          Parsetree.pexp_desc =
            (Parsetree.Pexp_apply
               ({
                  Parsetree.pexp_desc =
                    (Parsetree.Pexp_ident
                       {
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Ldot
                                  ((Longident.Lident "Earley_core"),
                                    "Earley")), f));
                         Asttypes.loc = _loc
                       });
                  Parsetree.pexp_loc = _loc;
                  Parsetree.pexp_attributes = []
                }, [(Asttypes.Nolabel, d); (Asttypes.Nolabel, e)]));
          Parsetree.pexp_loc = _loc;
          Parsetree.pexp_attributes = []
        } in
  let gn e f d =
    match d with
    | None ->
        {
          Parsetree.pexp_desc =
            (Parsetree.Pexp_apply
               ({
                  Parsetree.pexp_desc =
                    (Parsetree.Pexp_ident
                       {
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Ldot
                                  ((Longident.Lident "Earley_core"),
                                    "Earley")), "apply"));
                         Asttypes.loc = _loc
                       });
                  Parsetree.pexp_loc = _loc;
                  Parsetree.pexp_attributes = []
                },
                 [(Asttypes.Nolabel,
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_fun
                           (Asttypes.Nolabel, None,
                             {
                               Parsetree.ppat_desc =
                                 (Parsetree.Ppat_var
                                    { Asttypes.txt = "f"; Asttypes.loc = _loc
                                    });
                               Parsetree.ppat_loc = _loc;
                               Parsetree.ppat_attributes = []
                             },
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_apply
                                    ({
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_ident
                                            {
                                              Asttypes.txt =
                                                (Longident.Lident "f");
                                              Asttypes.loc = _loc
                                            });
                                       Parsetree.pexp_loc = _loc;
                                       Parsetree.pexp_attributes = []
                                     },
                                      [(Asttypes.Nolabel,
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_construct
                                                ({
                                                   Asttypes.txt =
                                                     (Longident.Lident "[]");
                                                   Asttypes.loc = _loc
                                                 }, None));
                                           Parsetree.pexp_loc = _loc;
                                           Parsetree.pexp_attributes = []
                                         })]));
                               Parsetree.pexp_loc = _loc;
                               Parsetree.pexp_attributes = []
                             }));
                      Parsetree.pexp_loc = _loc;
                      Parsetree.pexp_attributes = []
                    });
                 (Asttypes.Nolabel,
                   {
                     Parsetree.pexp_desc =
                       (Parsetree.Pexp_apply
                          ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_ident
                                  {
                                    Asttypes.txt =
                                      (Longident.Ldot
                                         ((Longident.Ldot
                                             ((Longident.Lident "Earley_core"),
                                               "Earley")), (f ^ "'")));
                                    Asttypes.loc = _loc
                                  });
                             Parsetree.pexp_loc = _loc;
                             Parsetree.pexp_attributes = []
                           },
                            [(Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_fun
                                      (Asttypes.Nolabel, None,
                                        {
                                          Parsetree.ppat_desc =
                                            (Parsetree.Ppat_var
                                               {
                                                 Asttypes.txt = "l";
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.ppat_loc = _loc;
                                          Parsetree.ppat_attributes = []
                                        },
                                        {
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Lident "l");
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        }));
                                 Parsetree.pexp_loc = _loc;
                                 Parsetree.pexp_attributes = []
                               });
                            (Asttypes.Nolabel, e);
                            (Asttypes.Nolabel,
                              {
                                Parsetree.pexp_desc =
                                  (Parsetree.Pexp_fun
                                     (Asttypes.Nolabel, None,
                                       {
                                         Parsetree.ppat_desc =
                                           (Parsetree.Ppat_var
                                              {
                                                Asttypes.txt = "x";
                                                Asttypes.loc = _loc
                                              });
                                         Parsetree.ppat_loc = _loc;
                                         Parsetree.ppat_attributes = []
                                       },
                                       {
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_fun
                                              (Asttypes.Nolabel, None,
                                                {
                                                  Parsetree.ppat_desc =
                                                    (Parsetree.Ppat_var
                                                       {
                                                         Asttypes.txt = "f";
                                                         Asttypes.loc = _loc
                                                       });
                                                  Parsetree.ppat_loc = _loc;
                                                  Parsetree.ppat_attributes =
                                                    []
                                                },
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_fun
                                                       (Asttypes.Nolabel,
                                                         None,
                                                         {
                                                           Parsetree.ppat_desc
                                                             =
                                                             (Parsetree.Ppat_var
                                                                {
                                                                  Asttypes.txt
                                                                    = "l";
                                                                  Asttypes.loc
                                                                    = _loc
                                                                });
                                                           Parsetree.ppat_loc
                                                             = _loc;
                                                           Parsetree.ppat_attributes
                                                             = []
                                                         },
                                                         {
                                                           Parsetree.pexp_desc
                                                             =
                                                             (Parsetree.Pexp_apply
                                                                ({
                                                                   Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "f");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                   Parsetree.pexp_loc
                                                                    = _loc;
                                                                   Parsetree.pexp_attributes
                                                                    = []
                                                                 },
                                                                  [(Asttypes.Nolabel,
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_construct
                                                                    ({
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "::");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    },
                                                                    (Some
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_tuple
                                                                    [
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "x");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    };
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_ident
                                                                    {
                                                                    Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "l");
                                                                    Asttypes.loc
                                                                    = _loc
                                                                    });
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    }]);
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    })));
                                                                    Parsetree.pexp_loc
                                                                    = _loc;
                                                                    Parsetree.pexp_attributes
                                                                    = []
                                                                    })]));
                                                           Parsetree.pexp_loc
                                                             = _loc;
                                                           Parsetree.pexp_attributes
                                                             = []
                                                         }));
                                                  Parsetree.pexp_loc = _loc;
                                                  Parsetree.pexp_attributes =
                                                    []
                                                }));
                                         Parsetree.pexp_loc = _loc;
                                         Parsetree.pexp_attributes = []
                                       }));
                                Parsetree.pexp_loc = _loc;
                                Parsetree.pexp_attributes = []
                              })]));
                     Parsetree.pexp_loc = _loc;
                     Parsetree.pexp_attributes = []
                   })]));
          Parsetree.pexp_loc = _loc;
          Parsetree.pexp_attributes = []
        }
    | Some d ->
        {
          Parsetree.pexp_desc =
            (Parsetree.Pexp_apply
               ({
                  Parsetree.pexp_desc =
                    (Parsetree.Pexp_ident
                       {
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Ldot
                                  ((Longident.Lident "Earley_core"),
                                    "Earley")), f));
                         Asttypes.loc = _loc
                       });
                  Parsetree.pexp_loc = _loc;
                  Parsetree.pexp_attributes = []
                }, [(Asttypes.Nolabel, d); (Asttypes.Nolabel, e)]));
          Parsetree.pexp_loc = _loc;
          Parsetree.pexp_attributes = []
        } in
  let kn e =
    function
    | None -> e
    | Some _ ->
        {
          Parsetree.pexp_desc =
            (Parsetree.Pexp_apply
               ({
                  Parsetree.pexp_desc =
                    (Parsetree.Pexp_ident
                       {
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Ldot
                                  ((Longident.Lident "Earley_core"),
                                    "Earley")), "greedy"));
                         Asttypes.loc = _loc
                       });
                  Parsetree.pexp_loc = _loc;
                  Parsetree.pexp_attributes = []
                }, [(Asttypes.Nolabel, e)]));
          Parsetree.pexp_loc = _loc;
          Parsetree.pexp_attributes = []
        } in
  match opt with
  | `Once -> e
  | `Option (d, g) -> kn (fn e "option" d) g
  | `Greedy ->
      {
        Parsetree.pexp_desc =
          (Parsetree.Pexp_apply
             ({
                Parsetree.pexp_desc =
                  (Parsetree.Pexp_ident
                     {
                       Asttypes.txt =
                         (Longident.Ldot
                            ((Longident.Ldot
                                ((Longident.Lident "Earley_core"), "Earley")),
                              "greedy"));
                       Asttypes.loc = _loc
                     });
                Parsetree.pexp_loc = _loc;
                Parsetree.pexp_attributes = []
              }, [(Asttypes.Nolabel, e)]));
        Parsetree.pexp_loc = _loc;
        Parsetree.pexp_attributes = []
      }
  | `Fixpoint (d, g) -> kn (gn e "fixpoint" d) g
  | `Fixpoint1 (d, g) -> kn (gn e "fixpoint1" d) g
let default_action _loc l =
  let l =
    List.filter
      (function | `Normal (("_", _), false, _, _, _) -> false | _ -> true) l in
  let l =
    List.map
      (function
       | `Normal ((id, _), _, _, _, _) ->
           {
             Parsetree.pexp_desc =
               (Parsetree.Pexp_ident
                  { Asttypes.txt = (Longident.Lident id); Asttypes.loc = _loc
                  });
             Parsetree.pexp_loc = _loc;
             Parsetree.pexp_attributes = []
           }
       | _ -> assert false) l in
  Pa_ast.exp_tuple _loc l
let from_opt ov d = match ov with | None -> d | Some v -> v
let dash =
  let fn str pos =
    let (c, str', pos') = Input.read str pos in
    if c = '-'
    then
      let (c', _, _) = Input.read str' pos' in
      (if c' = '>' then Earley.give_up () else ((), str', pos'))
    else Earley.give_up () in
  Earley.black_box fn (Charset.singleton '-') false "-"
module Ext(In:Extension) =
  struct
    include In
    let expr_arg = expression_lvl (NoMatch, (next_exp App))
    let build_rule (_loc, occur_loc, def, l, condition, action) =
      let (iter, action) =
        match action with
        | Normal a -> (false, a)
        | Default -> (false, (default_action _loc l))
        | DepSeq (def, cond, a) ->
            (true,
              ((match cond with
                | None -> def a
                | Some cond ->
                    def
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_ifthenelse
                             (cond, a,
                               (Some
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_apply
                                         ({
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Ldot
                                                        ((Longident.Ldot
                                                            ((Longident.Lident
                                                                "Earley_core"),
                                                              "Earley")),
                                                          "fail"));
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          },
                                           [(Asttypes.Nolabel,
                                              {
                                                Parsetree.pexp_desc =
                                                  (Parsetree.Pexp_construct
                                                     ({
                                                        Asttypes.txt =
                                                          (Longident.Lident
                                                             "()");
                                                        Asttypes.loc = _loc
                                                      }, None));
                                                Parsetree.pexp_loc = _loc;
                                                Parsetree.pexp_attributes =
                                                  []
                                              })]));
                                    Parsetree.pexp_loc = _loc;
                                    Parsetree.pexp_attributes = []
                                  })));
                        Parsetree.pexp_loc = _loc;
                        Parsetree.pexp_attributes = []
                      }))) in
      let rec fn ids l =
        match l with
        | [] ->
            let a = build_action _loc occur_loc ids action in
            let f =
              match ((find_locate ()), occur_loc) with
              | (Some _, true) -> "empty_pos"
              | _ -> "empty" in
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_apply
                   ({
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_ident
                           {
                             Asttypes.txt =
                               (Longident.Ldot
                                  ((Longident.Ldot
                                      ((Longident.Lident "Earley_core"),
                                        "Earley")), f));
                             Asttypes.loc = _loc
                           });
                      Parsetree.pexp_loc = _loc;
                      Parsetree.pexp_attributes = []
                    }, [(Asttypes.Nolabel, a)]));
              Parsetree.pexp_loc = _loc;
              Parsetree.pexp_attributes = []
            }
        | (`Normal (id, _, e, opt, occur_loc_id))::[] when
            match action.pexp_desc with
            | Pexp_ident { txt = Lident id' } when
                (ids = []) && ((fst id) = id') -> true
            | _ -> false ->
            (assert (not occur_loc);
             assert (not occur_loc_id);
             (let e = apply_option _loc opt e in e))
        | (`Normal (id, _, e, opt, occur_loc_id))::ls ->
            let e = apply_option _loc opt e in
            let a = fn ((id, occur_loc_id) :: ids) ls in
            let fn =
              match ((find_locate ()), occur_loc_id) with
              | (Some _, true) -> "fsequence_position"
              | _ when ((fst id) = "_") && ((snd id) = None) ->
                  "fsequence_ignore"
              | _ -> "fsequence" in
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_apply
                   ({
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_ident
                           {
                             Asttypes.txt =
                               (Longident.Ldot
                                  ((Longident.Ldot
                                      ((Longident.Lident "Earley_core"),
                                        "Earley")), fn));
                             Asttypes.loc = _loc
                           });
                      Parsetree.pexp_loc = _loc;
                      Parsetree.pexp_attributes = []
                    }, [(Asttypes.Nolabel, e); (Asttypes.Nolabel, a)]));
              Parsetree.pexp_loc = _loc;
              Parsetree.pexp_attributes = []
            } in
      let res = fn [] l in
      let res =
        if iter
        then
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Earley_core"),
                                      "Earley")), "iter"));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  }, [(Asttypes.Nolabel, res)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
        else res in
      (def, condition, res)
    let apply_def_cond _loc r =
      let (def, cond, e) = build_rule r in
      match cond with
      | None -> def e
      | Some c ->
          def
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_ifthenelse
                   (c, e,
                     (Some
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_apply
                               ({
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_ident
                                       {
                                         Asttypes.txt =
                                           (Longident.Ldot
                                              ((Longident.Ldot
                                                  ((Longident.Lident
                                                      "Earley_core"),
                                                    "Earley")), "fail"));
                                         Asttypes.loc = _loc
                                       });
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                },
                                 [(Asttypes.Nolabel,
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_construct
                                           ({
                                              Asttypes.txt =
                                                (Longident.Lident "()");
                                              Asttypes.loc = _loc
                                            }, None));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    })]));
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        })));
              Parsetree.pexp_loc = _loc;
              Parsetree.pexp_attributes = []
            }
    let apply_def_cond_list _loc r acc =
      let (def, cond, e) = build_rule r in
      match cond with
      | None ->
          def
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_construct
                   ({
                      Asttypes.txt = (Longident.Lident "::");
                      Asttypes.loc = _loc
                    },
                     (Some
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_tuple [e; acc]);
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        })));
              Parsetree.pexp_loc = _loc;
              Parsetree.pexp_attributes = []
            }
      | Some c ->
          def
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_apply
                   ({
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_ident
                           {
                             Asttypes.txt = (Longident.Lident "@");
                             Asttypes.loc = _loc
                           });
                      Parsetree.pexp_loc = _loc;
                      Parsetree.pexp_attributes = []
                    },
                     [(Asttypes.Nolabel,
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_ifthenelse
                               (c,
                                 {
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_construct
                                        ({
                                           Asttypes.txt =
                                             (Longident.Lident "::");
                                           Asttypes.loc = _loc
                                         },
                                          (Some
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_tuple
                                                    [e;
                                                    {
                                                      Parsetree.pexp_desc =
                                                        (Parsetree.Pexp_construct
                                                           ({
                                                              Asttypes.txt =
                                                                (Longident.Lident
                                                                   "[]");
                                                              Asttypes.loc =
                                                                _loc
                                                            }, None));
                                                      Parsetree.pexp_loc =
                                                        _loc;
                                                      Parsetree.pexp_attributes
                                                        = []
                                                    }]);
                                               Parsetree.pexp_loc = _loc;
                                               Parsetree.pexp_attributes = []
                                             })));
                                   Parsetree.pexp_loc = _loc;
                                   Parsetree.pexp_attributes = []
                                 },
                                 (Some
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_construct
                                           ({
                                              Asttypes.txt =
                                                (Longident.Lident "[]");
                                              Asttypes.loc = _loc
                                            }, None));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    })));
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        });
                     (Asttypes.Nolabel, acc)]));
              Parsetree.pexp_loc = _loc;
              Parsetree.pexp_attributes = []
            }
    let apply_def_cond_prio _loc arg r acc =
      let (def, cond, e) = build_rule r in
      match cond with
      | None ->
          def
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_construct
                   ({
                      Asttypes.txt = (Longident.Lident "::");
                      Asttypes.loc = _loc
                    },
                     (Some
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_tuple
                               [{
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_tuple
                                       [{
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_fun
                                               (Asttypes.Nolabel, None,
                                                 {
                                                   Parsetree.ppat_desc =
                                                     Parsetree.Ppat_any;
                                                   Parsetree.ppat_loc = _loc;
                                                   Parsetree.ppat_attributes
                                                     = []
                                                 },
                                                 {
                                                   Parsetree.pexp_desc =
                                                     (Parsetree.Pexp_construct
                                                        ({
                                                           Asttypes.txt =
                                                             (Longident.Lident
                                                                "true");
                                                           Asttypes.loc =
                                                             _loc
                                                         }, None));
                                                   Parsetree.pexp_loc = _loc;
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 }));
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        };
                                       e]);
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                };
                               acc]);
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        })));
              Parsetree.pexp_loc = _loc;
              Parsetree.pexp_attributes = []
            }
      | Some c ->
          def
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_construct
                   ({
                      Asttypes.txt = (Longident.Lident "::");
                      Asttypes.loc = _loc
                    },
                     (Some
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_tuple
                               [{
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_tuple
                                       [{
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_fun
                                               (Asttypes.Nolabel, None, arg,
                                                 c));
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        };
                                       e]);
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                };
                               acc]);
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        })));
              Parsetree.pexp_loc = _loc;
              Parsetree.pexp_attributes = []
            }
    let build_alternatives _loc ls =
      let ls = List.map snd ls in
      match ls with
      | [] ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Earley_core"),
                                      "Earley")), "fail"));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel,
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_construct
                             ({
                                Asttypes.txt = (Longident.Lident "()");
                                Asttypes.loc = _loc
                              }, None));
                        Parsetree.pexp_loc = _loc;
                        Parsetree.pexp_attributes = []
                      })]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
      | r::[] -> apply_def_cond _loc r
      | _::_::_ ->
          let l =
            List.fold_right (apply_def_cond_list _loc) ls
              {
                Parsetree.pexp_desc =
                  (Parsetree.Pexp_construct
                     ({
                        Asttypes.txt = (Longident.Lident "[]");
                        Asttypes.loc = _loc
                      }, None));
                Parsetree.pexp_loc = _loc;
                Parsetree.pexp_attributes = []
              } in
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Earley_core"),
                                      "Earley")), "alternatives"));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  }, [(Asttypes.Nolabel, l)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
    let build_prio_alternatives _loc arg ls =
      let (l0, l1) = List.partition fst ls in
      let l0 = List.map snd l0
      and l1 = List.map snd l1 in
      let l1 =
        List.fold_right (apply_def_cond_prio _loc arg) l1
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_construct
                 ({
                    Asttypes.txt = (Longident.Lident "[]");
                    Asttypes.loc = _loc
                  }, None));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          } in
      let l0 =
        List.fold_right (apply_def_cond_list _loc) l0
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_construct
                 ({
                    Asttypes.txt = (Longident.Lident "[]");
                    Asttypes.loc = _loc
                  }, None));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          } in
      {
        Parsetree.pexp_desc =
          (Parsetree.Pexp_tuple
             [l1;
             {
               Parsetree.pexp_desc =
                 (Parsetree.Pexp_fun (Asttypes.Nolabel, None, arg, l0));
               Parsetree.pexp_loc = _loc;
               Parsetree.pexp_attributes = []
             }]);
        Parsetree.pexp_loc = _loc;
        Parsetree.pexp_attributes = []
      }
    let build_str_item _loc l =
      let rec fn =
        function
        | [] -> ([], [], [])
        | (`Caml b)::l ->
            let (str1, str2, str3) = fn l in (str1, str2, (b :: str3))
        | (`Parser (name, args, prio, ty, _loc_r, r))::l ->
            let (str1, str2, str3) = fn l in
            let pname =
              {
                Parsetree.ppat_desc =
                  (Parsetree.Ppat_var
                     { Asttypes.txt = name; Asttypes.loc = _loc });
                Parsetree.ppat_loc = _loc;
                Parsetree.ppat_attributes = []
              } in
            let coer f =
              match ty with
              | None -> f
              | Some ty ->
                  {
                    Parsetree.pexp_desc = (Parsetree.Pexp_constraint (f, ty));
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  } in
            let args_pat = Pa_ast.pat_tuple _loc args in
            let (str1, str2) =
              match (args, prio) with
              | ([], None) ->
                  let r = coer (build_alternatives _loc_r r) in
                  (([{
                       Parsetree.pstr_desc =
                         (Parsetree.Pstr_value
                            (Asttypes.Nonrecursive,
                              [{
                                 Parsetree.pvb_pat = pname;
                                 Parsetree.pvb_expr =
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_apply
                                          ({
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_ident
                                                  {
                                                    Asttypes.txt =
                                                      (Longident.Ldot
                                                         ((Longident.Ldot
                                                             ((Longident.Lident
                                                                 "Earley_core"),
                                                               "Earley")),
                                                           "declare_grammar"));
                                                    Asttypes.loc = _loc
                                                  });
                                             Parsetree.pexp_loc = _loc;
                                             Parsetree.pexp_attributes = []
                                           },
                                            [(Asttypes.Nolabel,
                                               (Pa_ast.exp_string _loc name))]));
                                     Parsetree.pexp_loc = _loc;
                                     Parsetree.pexp_attributes = []
                                   };
                                 Parsetree.pvb_attributes = [];
                                 Parsetree.pvb_loc = _loc
                               }]));
                       Parsetree.pstr_loc = _loc
                     }] @ str1),
                    ([{
                        Parsetree.pstr_desc =
                          (Parsetree.Pstr_value
                             (Asttypes.Nonrecursive,
                               [{
                                  Parsetree.pvb_pat =
                                    {
                                      Parsetree.ppat_desc =
                                        Parsetree.Ppat_any;
                                      Parsetree.ppat_loc = _loc;
                                      Parsetree.ppat_attributes = []
                                    };
                                  Parsetree.pvb_expr =
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_apply
                                           ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_ident
                                                   {
                                                     Asttypes.txt =
                                                       (Longident.Ldot
                                                          ((Longident.Ldot
                                                              ((Longident.Lident
                                                                  "Earley_core"),
                                                                "Earley")),
                                                            "set_grammar"));
                                                     Asttypes.loc = _loc
                                                   });
                                              Parsetree.pexp_loc = _loc;
                                              Parsetree.pexp_attributes = []
                                            },
                                             [(Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_ident
                                                       {
                                                         Asttypes.txt =
                                                           (Longident.Lident
                                                              name);
                                                         Asttypes.loc = _loc
                                                       });
                                                  Parsetree.pexp_loc = _loc;
                                                  Parsetree.pexp_attributes =
                                                    []
                                                });
                                             (Asttypes.Nolabel, r)]));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    };
                                  Parsetree.pvb_attributes = [];
                                  Parsetree.pvb_loc = _loc
                                }]));
                        Parsetree.pstr_loc = _loc
                      }] @ str2))
              | (_, None) ->
                  let r = coer (build_alternatives _loc_r r) in
                  let set_name = name ^ "__set__grammar" in
                  (([{
                       Parsetree.pstr_desc =
                         (Parsetree.Pstr_value
                            (Asttypes.Nonrecursive,
                              [{
                                 Parsetree.pvb_pat =
                                   {
                                     Parsetree.ppat_desc =
                                       (Parsetree.Ppat_tuple
                                          [pname;
                                          {
                                            Parsetree.ppat_desc =
                                              (Parsetree.Ppat_var
                                                 {
                                                   Asttypes.txt = set_name;
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.ppat_loc = _loc;
                                            Parsetree.ppat_attributes = []
                                          }]);
                                     Parsetree.ppat_loc = _loc;
                                     Parsetree.ppat_attributes = []
                                   };
                                 Parsetree.pvb_expr =
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_apply
                                          ({
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_ident
                                                  {
                                                    Asttypes.txt =
                                                      (Longident.Ldot
                                                         ((Longident.Ldot
                                                             ((Longident.Lident
                                                                 "Earley_core"),
                                                               "Earley")),
                                                           "grammar_family"));
                                                    Asttypes.loc = _loc
                                                  });
                                             Parsetree.pexp_loc = _loc;
                                             Parsetree.pexp_attributes = []
                                           },
                                            [(Asttypes.Nolabel,
                                               (Pa_ast.exp_string _loc name))]));
                                     Parsetree.pexp_loc = _loc;
                                     Parsetree.pexp_attributes = []
                                   };
                                 Parsetree.pvb_attributes = [];
                                 Parsetree.pvb_loc = _loc
                               }]));
                       Parsetree.pstr_loc = _loc
                     }] @ str1),
                    ([{
                        Parsetree.pstr_desc =
                          (Parsetree.Pstr_value
                             (Asttypes.Nonrecursive,
                               [{
                                  Parsetree.pvb_pat =
                                    {
                                      Parsetree.ppat_desc =
                                        Parsetree.Ppat_any;
                                      Parsetree.ppat_loc = _loc;
                                      Parsetree.ppat_attributes = []
                                    };
                                  Parsetree.pvb_expr =
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_apply
                                           ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_ident
                                                   {
                                                     Asttypes.txt =
                                                       (Longident.Lident
                                                          set_name);
                                                     Asttypes.loc = _loc
                                                   });
                                              Parsetree.pexp_loc = _loc;
                                              Parsetree.pexp_attributes = []
                                            },
                                             [(Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_fun
                                                       (Asttypes.Nolabel,
                                                         None, args_pat, r));
                                                  Parsetree.pexp_loc = _loc;
                                                  Parsetree.pexp_attributes =
                                                    []
                                                })]));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    };
                                  Parsetree.pvb_attributes = [];
                                  Parsetree.pvb_loc = _loc
                                }]));
                        Parsetree.pstr_loc = _loc
                      }] @ str2))
              | ([], Some prio) ->
                  let r = coer (build_prio_alternatives _loc_r prio r) in
                  let set_name = name ^ "__set__grammar" in
                  (([{
                       Parsetree.pstr_desc =
                         (Parsetree.Pstr_value
                            (Asttypes.Nonrecursive,
                              [{
                                 Parsetree.pvb_pat =
                                   {
                                     Parsetree.ppat_desc =
                                       (Parsetree.Ppat_tuple
                                          [pname;
                                          {
                                            Parsetree.ppat_desc =
                                              (Parsetree.Ppat_var
                                                 {
                                                   Asttypes.txt = set_name;
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.ppat_loc = _loc;
                                            Parsetree.ppat_attributes = []
                                          }]);
                                     Parsetree.ppat_loc = _loc;
                                     Parsetree.ppat_attributes = []
                                   };
                                 Parsetree.pvb_expr =
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_apply
                                          ({
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_ident
                                                  {
                                                    Asttypes.txt =
                                                      (Longident.Ldot
                                                         ((Longident.Ldot
                                                             ((Longident.Lident
                                                                 "Earley_core"),
                                                               "Earley")),
                                                           "grammar_prio"));
                                                    Asttypes.loc = _loc
                                                  });
                                             Parsetree.pexp_loc = _loc;
                                             Parsetree.pexp_attributes = []
                                           },
                                            [(Asttypes.Nolabel,
                                               (Pa_ast.exp_string _loc name))]));
                                     Parsetree.pexp_loc = _loc;
                                     Parsetree.pexp_attributes = []
                                   };
                                 Parsetree.pvb_attributes = [];
                                 Parsetree.pvb_loc = _loc
                               }]));
                       Parsetree.pstr_loc = _loc
                     }] @ str1),
                    ([{
                        Parsetree.pstr_desc =
                          (Parsetree.Pstr_value
                             (Asttypes.Nonrecursive,
                               [{
                                  Parsetree.pvb_pat =
                                    {
                                      Parsetree.ppat_desc =
                                        Parsetree.Ppat_any;
                                      Parsetree.ppat_loc = _loc;
                                      Parsetree.ppat_attributes = []
                                    };
                                  Parsetree.pvb_expr =
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_apply
                                           ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_ident
                                                   {
                                                     Asttypes.txt =
                                                       (Longident.Lident
                                                          set_name);
                                                     Asttypes.loc = _loc
                                                   });
                                              Parsetree.pexp_loc = _loc;
                                              Parsetree.pexp_attributes = []
                                            }, [(Asttypes.Nolabel, r)]));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    };
                                  Parsetree.pvb_attributes = [];
                                  Parsetree.pvb_loc = _loc
                                }]));
                        Parsetree.pstr_loc = _loc
                      }] @ str2))
              | (args, Some prio) ->
                  let r = coer (build_prio_alternatives _loc_r prio r) in
                  let set_name = name ^ "__set__grammar" in
                  (([{
                       Parsetree.pstr_desc =
                         (Parsetree.Pstr_value
                            (Asttypes.Nonrecursive,
                              [{
                                 Parsetree.pvb_pat =
                                   {
                                     Parsetree.ppat_desc =
                                       (Parsetree.Ppat_tuple
                                          [pname;
                                          {
                                            Parsetree.ppat_desc =
                                              (Parsetree.Ppat_var
                                                 {
                                                   Asttypes.txt = set_name;
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.ppat_loc = _loc;
                                            Parsetree.ppat_attributes = []
                                          }]);
                                     Parsetree.ppat_loc = _loc;
                                     Parsetree.ppat_attributes = []
                                   };
                                 Parsetree.pvb_expr =
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_apply
                                          ({
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_ident
                                                  {
                                                    Asttypes.txt =
                                                      (Longident.Ldot
                                                         ((Longident.Ldot
                                                             ((Longident.Lident
                                                                 "Earley_core"),
                                                               "Earley")),
                                                           "grammar_prio_family"));
                                                    Asttypes.loc = _loc
                                                  });
                                             Parsetree.pexp_loc = _loc;
                                             Parsetree.pexp_attributes = []
                                           },
                                            [(Asttypes.Nolabel,
                                               (Pa_ast.exp_string _loc name))]));
                                     Parsetree.pexp_loc = _loc;
                                     Parsetree.pexp_attributes = []
                                   };
                                 Parsetree.pvb_attributes = [];
                                 Parsetree.pvb_loc = _loc
                               }]));
                       Parsetree.pstr_loc = _loc
                     }] @ str1),
                    ([{
                        Parsetree.pstr_desc =
                          (Parsetree.Pstr_value
                             (Asttypes.Nonrecursive,
                               [{
                                  Parsetree.pvb_pat =
                                    {
                                      Parsetree.ppat_desc =
                                        Parsetree.Ppat_any;
                                      Parsetree.ppat_loc = _loc;
                                      Parsetree.ppat_attributes = []
                                    };
                                  Parsetree.pvb_expr =
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_apply
                                           ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_ident
                                                   {
                                                     Asttypes.txt =
                                                       (Longident.Lident
                                                          set_name);
                                                     Asttypes.loc = _loc
                                                   });
                                              Parsetree.pexp_loc = _loc;
                                              Parsetree.pexp_attributes = []
                                            },
                                             [(Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_fun
                                                       (Asttypes.Nolabel,
                                                         None, args_pat, r));
                                                  Parsetree.pexp_loc = _loc;
                                                  Parsetree.pexp_attributes =
                                                    []
                                                })]));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    };
                                  Parsetree.pvb_attributes = [];
                                  Parsetree.pvb_loc = _loc
                                }]));
                        Parsetree.pstr_loc = _loc
                      }] @ str2)) in
            let str2 =
              match (args, prio) with
              | ([], _)|(_::[], None) -> str2
              | _ ->
                  let rec currify acc n =
                    function
                    | [] ->
                        (match prio with
                         | None ->
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_apply
                                    ({
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_ident
                                            {
                                              Asttypes.txt =
                                                (Longident.Lident name);
                                              Asttypes.loc = _loc
                                            });
                                       Parsetree.pexp_loc = _loc;
                                       Parsetree.pexp_attributes = []
                                     },
                                      [(Asttypes.Nolabel,
                                         (Pa_ast.exp_tuple _loc
                                            (List.rev acc)))]));
                               Parsetree.pexp_loc = _loc;
                               Parsetree.pexp_attributes = []
                             }
                         | Some _ ->
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_fun
                                    (Asttypes.Nolabel, None,
                                      {
                                        Parsetree.ppat_desc =
                                          (Parsetree.Ppat_var
                                             {
                                               Asttypes.txt = "__curry__prio";
                                               Asttypes.loc = _loc
                                             });
                                        Parsetree.ppat_loc = _loc;
                                        Parsetree.ppat_attributes = []
                                      },
                                      {
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_apply
                                             ({
                                                Parsetree.pexp_desc =
                                                  (Parsetree.Pexp_ident
                                                     {
                                                       Asttypes.txt =
                                                         (Longident.Lident
                                                            name);
                                                       Asttypes.loc = _loc
                                                     });
                                                Parsetree.pexp_loc = _loc;
                                                Parsetree.pexp_attributes =
                                                  []
                                              },
                                               [(Asttypes.Nolabel,
                                                  (Pa_ast.exp_tuple _loc
                                                     (List.rev acc)));
                                               (Asttypes.Nolabel,
                                                 {
                                                   Parsetree.pexp_desc =
                                                     (Parsetree.Pexp_ident
                                                        {
                                                          Asttypes.txt =
                                                            (Longident.Lident
                                                               "__curry__prio");
                                                          Asttypes.loc = _loc
                                                        });
                                                   Parsetree.pexp_loc = _loc;
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 })]));
                                        Parsetree.pexp_loc = _loc;
                                        Parsetree.pexp_attributes = []
                                      }));
                               Parsetree.pexp_loc = _loc;
                               Parsetree.pexp_attributes = []
                             })
                    | a::l ->
                        let v = "__curry__varx" ^ (string_of_int n) in
                        let acc =
                          {
                            Parsetree.pexp_desc =
                              (Parsetree.Pexp_ident
                                 {
                                   Asttypes.txt = (Longident.Lident v);
                                   Asttypes.loc = _loc
                                 });
                            Parsetree.pexp_loc = _loc;
                            Parsetree.pexp_attributes = []
                          } :: acc in
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_fun
                               (Asttypes.Nolabel, None,
                                 {
                                   Parsetree.ppat_desc =
                                     (Parsetree.Ppat_var
                                        {
                                          Asttypes.txt = v;
                                          Asttypes.loc = _loc
                                        });
                                   Parsetree.ppat_loc = _loc;
                                   Parsetree.ppat_attributes = []
                                 }, (currify acc (n + 1) l)));
                          Parsetree.pexp_loc = _loc;
                          Parsetree.pexp_attributes = []
                        } in
                  let f = currify [] 0 args in
                  [{
                     Parsetree.pstr_desc =
                       (Parsetree.Pstr_value
                          (Asttypes.Nonrecursive,
                            [{
                               Parsetree.pvb_pat = pname;
                               Parsetree.pvb_expr = f;
                               Parsetree.pvb_attributes = [];
                               Parsetree.pvb_loc = _loc
                             }]));
                     Parsetree.pstr_loc = _loc
                   }] @ str2 in
            (str1, str2, str3) in
      let (str1, str2, str3) = fn l in
      if str3 = []
      then str1 @ str2
      else
        str1 @
          ([{
              Parsetree.pstr_desc =
                (Parsetree.Pstr_value (Asttypes.Recursive, str3));
              Parsetree.pstr_loc = _loc
            }] @ str2)
    let glr_sequence = Earley_core.Earley.declare_grammar "glr_sequence"
    let glr_opt_expr = Earley_core.Earley.declare_grammar "glr_opt_expr"
    let glr_option = Earley_core.Earley.declare_grammar "glr_option"
    let glr_ident = Earley_core.Earley.declare_grammar "glr_ident"
    let glr_left_member =
      Earley_core.Earley.declare_grammar "glr_left_member"
    let glr_let = Earley_core.Earley.declare_grammar "glr_let"
    let glr_cond = Earley_core.Earley.declare_grammar "glr_cond"
    let (glr_action, glr_action__set__grammar) =
      Earley_core.Earley.grammar_family "glr_action"
    let (glr_rule, glr_rule__set__grammar) =
      Earley_core.Earley.grammar_family "glr_rule"
    let (glr_at_rule, glr_at_rule__set__grammar) =
      Earley_core.Earley.grammar_family "glr_at_rule"
    let glr_rules = Earley_core.Earley.declare_grammar "glr_rules"
    let _ =
      Earley_core.Earley.set_grammar glr_sequence
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "(" "(")
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string ")" ")")
                    (Earley_core.Earley.empty (fun e -> (true, e)))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '{' '{')
             (Earley_core.Earley.fsequence_position glr_rules
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '}' '}')
                   (Earley_core.Earley.empty
                      (fun str ->
                         fun pos ->
                           fun str' ->
                             fun pos' ->
                               fun r ->
                                 let _loc_r = locate str pos str' pos' in
                                 (true, (build_alternatives _loc_r r))))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "EOF" "EOF")
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun oe ->
                              ((oe <> None),
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_apply
                                       ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Ldot
                                                      ((Longident.Ldot
                                                          ((Longident.Lident
                                                              "Earley_core"),
                                                            "Earley")),
                                                        "eof"));
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        },
                                         [(Asttypes.Nolabel,
                                            (from_opt oe
                                               {
                                                 Parsetree.pexp_desc =
                                                   (Parsetree.Pexp_construct
                                                      ({
                                                         Asttypes.txt =
                                                           (Longident.Lident
                                                              "()");
                                                         Asttypes.loc = _loc
                                                       }, None));
                                                 Parsetree.pexp_loc = _loc;
                                                 Parsetree.pexp_attributes =
                                                   []
                                               }))]));
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                }))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "EMPTY" "EMPTY")
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun oe ->
                              ((oe <> None),
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_apply
                                       ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Ldot
                                                      ((Longident.Ldot
                                                          ((Longident.Lident
                                                              "Earley_core"),
                                                            "Earley")),
                                                        "empty"));
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        },
                                         [(Asttypes.Nolabel,
                                            (from_opt oe
                                               {
                                                 Parsetree.pexp_desc =
                                                   (Parsetree.Pexp_construct
                                                      ({
                                                         Asttypes.txt =
                                                           (Longident.Lident
                                                              "()");
                                                         Asttypes.loc = _loc
                                                       }, None));
                                                 Parsetree.pexp_loc = _loc;
                                                 Parsetree.pexp_attributes =
                                                   []
                                               }))]));
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                }))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "FAIL" "FAIL")
             (Earley_core.Earley.fsequence expr_arg
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun e ->
                              (false,
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_apply
                                       ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Ldot
                                                      ((Longident.Ldot
                                                          ((Longident.Lident
                                                              "Earley_core"),
                                                            "Earley")),
                                                        "fail"));
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        }, [(Asttypes.Nolabel, e)]));
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                }))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "DEBUG" "DEBUG")
             (Earley_core.Earley.fsequence expr_arg
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun e ->
                              (false,
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_apply
                                       ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Ldot
                                                      ((Longident.Ldot
                                                          ((Longident.Lident
                                                              "Earley_core"),
                                                            "Earley")),
                                                        "debug"));
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        }, [(Asttypes.Nolabel, e)]));
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                }))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "ANY" "ANY")
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         (true,
                           {
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_ident
                                  {
                                    Asttypes.txt =
                                      (Longident.Ldot
                                         ((Longident.Ldot
                                             ((Longident.Lident "Earley_core"),
                                               "Earley")), "any"));
                                    Asttypes.loc = _loc
                                  });
                             Parsetree.pexp_loc = _loc;
                             Parsetree.pexp_attributes = []
                           })));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "CHR" "CHR")
             (Earley_core.Earley.fsequence expr_arg
                (Earley_core.Earley.fsequence glr_opt_expr
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun oe ->
                                 fun e ->
                                   ((oe <> None),
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_apply
                                            ({
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_ident
                                                    {
                                                      Asttypes.txt =
                                                        (Longident.Ldot
                                                           ((Longident.Ldot
                                                               ((Longident.Lident
                                                                   "Earley_core"),
                                                                 "Earley")),
                                                             "char"));
                                                      Asttypes.loc = _loc
                                                    });
                                               Parsetree.pexp_loc = _loc;
                                               Parsetree.pexp_attributes = []
                                             },
                                              [(Asttypes.Nolabel, e);
                                              (Asttypes.Nolabel,
                                                (from_opt oe e))]));
                                       Parsetree.pexp_loc = _loc;
                                       Parsetree.pexp_attributes = []
                                     })))));
           Earley_core.Earley.fsequence char_litteral
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun oe ->
                              fun c ->
                                let e = Pa_ast.exp_char _loc c in
                                ((oe <> None),
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_apply
                                         ({
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Ldot
                                                        ((Longident.Ldot
                                                            ((Longident.Lident
                                                                "Earley_core"),
                                                              "Earley")),
                                                          "char"));
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          },
                                           [(Asttypes.Nolabel, e);
                                           (Asttypes.Nolabel,
                                             (from_opt oe e))]));
                                    Parsetree.pexp_loc = _loc;
                                    Parsetree.pexp_attributes = []
                                  }))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "STR" "STR")
             (Earley_core.Earley.fsequence expr_arg
                (Earley_core.Earley.fsequence glr_opt_expr
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun oe ->
                                 fun e ->
                                   ((oe <> None),
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_apply
                                            ({
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_ident
                                                    {
                                                      Asttypes.txt =
                                                        (Longident.Ldot
                                                           ((Longident.Ldot
                                                               ((Longident.Lident
                                                                   "Earley_core"),
                                                                 "Earley")),
                                                             "string"));
                                                      Asttypes.loc = _loc
                                                    });
                                               Parsetree.pexp_loc = _loc;
                                               Parsetree.pexp_attributes = []
                                             },
                                              [(Asttypes.Nolabel, e);
                                              (Asttypes.Nolabel,
                                                (from_opt oe e))]));
                                       Parsetree.pexp_loc = _loc;
                                       Parsetree.pexp_attributes = []
                                     })))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "ERROR" "ERROR")
             (Earley_core.Earley.fsequence expr_arg
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun e ->
                              (true,
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_apply
                                       ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Ldot
                                                      ((Longident.Ldot
                                                          ((Longident.Lident
                                                              "Earley_core"),
                                                            "Earley")),
                                                        "error_message"));
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        },
                                         [(Asttypes.Nolabel,
                                            {
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_fun
                                                   (Asttypes.Nolabel, None,
                                                     {
                                                       Parsetree.ppat_desc =
                                                         (Parsetree.Ppat_construct
                                                            ({
                                                               Asttypes.txt =
                                                                 (Longident.Lident
                                                                    "()");
                                                               Asttypes.loc =
                                                                 _loc
                                                             }, None));
                                                       Parsetree.ppat_loc =
                                                         _loc;
                                                       Parsetree.ppat_attributes
                                                         = []
                                                     }, e));
                                              Parsetree.pexp_loc = _loc;
                                              Parsetree.pexp_attributes = []
                                            })]));
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                }))));
           Earley_core.Earley.fsequence string_litteral
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun oe ->
                              fun ((s, _) as _default_0) ->
                                if (String.length s) = 0
                                then Earley.give_up ();
                                (let s = Pa_ast.exp_string _loc s in
                                 let e = from_opt oe s in
                                 ((oe <> None),
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_apply
                                          ({
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_ident
                                                  {
                                                    Asttypes.txt =
                                                      (Longident.Ldot
                                                         ((Longident.Ldot
                                                             ((Longident.Lident
                                                                 "Earley_core"),
                                                               "Earley")),
                                                           "string"));
                                                    Asttypes.loc = _loc
                                                  });
                                             Parsetree.pexp_loc = _loc;
                                             Parsetree.pexp_attributes = []
                                           },
                                            [(Asttypes.Nolabel, s);
                                            (Asttypes.Nolabel, e)]));
                                     Parsetree.pexp_loc = _loc;
                                     Parsetree.pexp_attributes = []
                                   })))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "RE" "RE")
             (Earley_core.Earley.fsequence expr_arg
                (Earley_core.Earley.fsequence glr_opt_expr
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun opt ->
                                 fun e ->
                                   let act =
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_fun
                                            (Asttypes.Nolabel, None,
                                              {
                                                Parsetree.ppat_desc =
                                                  (Parsetree.Ppat_var
                                                     {
                                                       Asttypes.txt =
                                                         "groupe";
                                                       Asttypes.loc = _loc
                                                     });
                                                Parsetree.ppat_loc = _loc;
                                                Parsetree.ppat_attributes =
                                                  []
                                              },
                                              (from_opt opt
                                                 {
                                                   Parsetree.pexp_desc =
                                                     (Parsetree.Pexp_apply
                                                        ({
                                                           Parsetree.pexp_desc
                                                             =
                                                             (Parsetree.Pexp_ident
                                                                {
                                                                  Asttypes.txt
                                                                    =
                                                                    (
                                                                    Longident.Lident
                                                                    "groupe");
                                                                  Asttypes.loc
                                                                    = _loc
                                                                });
                                                           Parsetree.pexp_loc
                                                             = _loc;
                                                           Parsetree.pexp_attributes
                                                             = []
                                                         },
                                                          [(Asttypes.Nolabel,
                                                             {
                                                               Parsetree.pexp_desc
                                                                 =
                                                                 (Parsetree.Pexp_constant
                                                                    (
                                                                    Parsetree.Pconst_integer
                                                                    ("0",
                                                                    None)));
                                                               Parsetree.pexp_loc
                                                                 = _loc;
                                                               Parsetree.pexp_attributes
                                                                 = []
                                                             })]));
                                                   Parsetree.pexp_loc = _loc;
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 })));
                                       Parsetree.pexp_loc = _loc;
                                       Parsetree.pexp_attributes = []
                                     } in
                                   match e.pexp_desc with
                                   | Pexp_ident { txt = Lident id } ->
                                       let id =
                                         let l = String.length id in
                                         if
                                           (l > 3) &&
                                             ((String.sub id (l - 3) 3) =
                                                "_re")
                                         then String.sub id 0 (l - 3)
                                         else id in
                                       (true,
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_apply
                                                ({
                                                   Parsetree.pexp_desc =
                                                     (Parsetree.Pexp_ident
                                                        {
                                                          Asttypes.txt =
                                                            (Longident.Ldot
                                                               ((Longident.Lident
                                                                   "Earley_str"),
                                                                 "regexp"));
                                                          Asttypes.loc = _loc
                                                        });
                                                   Parsetree.pexp_loc = _loc;
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [((Asttypes.Labelled "name"),
                                                     (Pa_ast.exp_string _loc
                                                        id));
                                                  (Asttypes.Nolabel, e);
                                                  (Asttypes.Nolabel, act)]));
                                           Parsetree.pexp_loc = _loc;
                                           Parsetree.pexp_attributes = []
                                         })
                                   | _ ->
                                       (true,
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_apply
                                                ({
                                                   Parsetree.pexp_desc =
                                                     (Parsetree.Pexp_ident
                                                        {
                                                          Asttypes.txt =
                                                            (Longident.Ldot
                                                               ((Longident.Lident
                                                                   "Earley_str"),
                                                                 "regexp"));
                                                          Asttypes.loc = _loc
                                                        });
                                                   Parsetree.pexp_loc = _loc;
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [(Asttypes.Nolabel, e);
                                                  (Asttypes.Nolabel, act)]));
                                           Parsetree.pexp_loc = _loc;
                                           Parsetree.pexp_attributes = []
                                         })))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "BLANK" "BLANK")
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun oe ->
                              let e =
                                from_opt oe
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_construct
                                         ({
                                            Asttypes.txt =
                                              (Longident.Lident "()");
                                            Asttypes.loc = _loc
                                          }, None));
                                    Parsetree.pexp_loc = _loc;
                                    Parsetree.pexp_attributes = []
                                  } in
                              ((oe <> None),
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_apply
                                       ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Ldot
                                                      ((Longident.Ldot
                                                          ((Longident.Lident
                                                              "Earley_core"),
                                                            "Earley")),
                                                        "with_blank_test"));
                                                 Asttypes.loc = _loc
                                               });
                                          Parsetree.pexp_loc = _loc;
                                          Parsetree.pexp_attributes = []
                                        }, [(Asttypes.Nolabel, e)]));
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                }))));
           Earley_core.Earley.fsequence dash
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun oe ->
                              fun _default_0 ->
                                let e =
                                  from_opt oe
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_construct
                                           ({
                                              Asttypes.txt =
                                                (Longident.Lident "()");
                                              Asttypes.loc = _loc
                                            }, None));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    } in
                                ((oe <> None),
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_apply
                                         ({
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Ldot
                                                        ((Longident.Ldot
                                                            ((Longident.Lident
                                                                "Earley_core"),
                                                              "Earley")),
                                                          "no_blank_test"));
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          }, [(Asttypes.Nolabel, e)]));
                                    Parsetree.pexp_loc = _loc;
                                    Parsetree.pexp_attributes = []
                                  }))));
           Earley_core.Earley.fsequence regexp_litteral
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun oe ->
                              fun s ->
                                let opt =
                                  from_opt oe
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_apply
                                           ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_ident
                                                   {
                                                     Asttypes.txt =
                                                       (Longident.Lident
                                                          "groupe");
                                                     Asttypes.loc = _loc
                                                   });
                                              Parsetree.pexp_loc = _loc;
                                              Parsetree.pexp_attributes = []
                                            },
                                             [(Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_constant
                                                       (Parsetree.Pconst_integer
                                                          ("0", None)));
                                                  Parsetree.pexp_loc = _loc;
                                                  Parsetree.pexp_attributes =
                                                    []
                                                })]));
                                      Parsetree.pexp_loc = _loc;
                                      Parsetree.pexp_attributes = []
                                    } in
                                let es = String.escaped s in
                                let act =
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_fun
                                         (Asttypes.Nolabel, None,
                                           {
                                             Parsetree.ppat_desc =
                                               (Parsetree.Ppat_var
                                                  {
                                                    Asttypes.txt = "groupe";
                                                    Asttypes.loc = _loc
                                                  });
                                             Parsetree.ppat_loc = _loc;
                                             Parsetree.ppat_attributes = []
                                           }, opt));
                                    Parsetree.pexp_loc = _loc;
                                    Parsetree.pexp_attributes = []
                                  } in
                                (true,
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_apply
                                         ({
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Ldot
                                                        ((Longident.Lident
                                                            "Earley_str"),
                                                          "regexp"));
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          },
                                           [((Asttypes.Labelled "name"),
                                              (Pa_ast.exp_string _loc es));
                                           (Asttypes.Nolabel,
                                             (Pa_ast.exp_string _loc s));
                                           (Asttypes.Nolabel, act)]));
                                    Parsetree.pexp_loc = _loc;
                                    Parsetree.pexp_attributes = []
                                  }))));
           Earley_core.Earley.fsequence new_regexp_litteral
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun opt ->
                              fun s ->
                                let es = String.escaped s in
                                let s = "\\(" ^ (s ^ "\\)") in
                                let re =
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_apply
                                         ({
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Ldot
                                                        ((Longident.Ldot
                                                            ((Longident.Lident
                                                                "Earley_core"),
                                                              "Earley")),
                                                          "regexp"));
                                                   Asttypes.loc = _loc
                                                 });
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          },
                                           [((Asttypes.Labelled "name"),
                                              (Pa_ast.exp_string _loc es));
                                           (Asttypes.Nolabel,
                                             (Pa_ast.exp_string _loc s))]));
                                    Parsetree.pexp_loc = _loc;
                                    Parsetree.pexp_attributes = []
                                  } in
                                match opt with
                                | None -> (true, re)
                                | Some e ->
                                    (true,
                                      {
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_apply
                                             ({
                                                Parsetree.pexp_desc =
                                                  (Parsetree.Pexp_ident
                                                     {
                                                       Asttypes.txt =
                                                         (Longident.Ldot
                                                            ((Longident.Ldot
                                                                ((Longident.Lident
                                                                    "Earley_core"),
                                                                  "Earley")),
                                                              "apply"));
                                                       Asttypes.loc = _loc
                                                     });
                                                Parsetree.pexp_loc = _loc;
                                                Parsetree.pexp_attributes =
                                                  []
                                              },
                                               [(Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc =
                                                      (Parsetree.Pexp_fun
                                                         (Asttypes.Nolabel,
                                                           None,
                                                           {
                                                             Parsetree.ppat_desc
                                                               =
                                                               (Parsetree.Ppat_var
                                                                  {
                                                                    Asttypes.txt
                                                                    = "group";
                                                                    Asttypes.loc
                                                                    = _loc
                                                                  });
                                                             Parsetree.ppat_loc
                                                               = _loc;
                                                             Parsetree.ppat_attributes
                                                               = []
                                                           }, e));
                                                    Parsetree.pexp_loc = _loc;
                                                    Parsetree.pexp_attributes
                                                      = []
                                                  });
                                               (Asttypes.Nolabel, re)]));
                                        Parsetree.pexp_loc = _loc;
                                        Parsetree.pexp_attributes = []
                                      }))));
           Earley_core.Earley.fsequence value_path
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun id ->
                           (true,
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_ident
                                    { Asttypes.txt = id; Asttypes.loc = _loc
                                    });
                               Parsetree.pexp_loc = _loc;
                               Parsetree.pexp_attributes = []
                             })))])
    let _ =
      Earley_core.Earley.set_grammar glr_opt_expr
        (Earley_core.Earley.option None
           (Earley_core.Earley.apply (fun x -> Some x)
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '[' '[')
                 (Earley_core.Earley.fsequence expression
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ']' ']')
                       (Earley_core.Earley.empty
                          (fun _default_0 -> _default_0)))))))
    let _ =
      Earley_core.Earley.set_grammar glr_option
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty `Once);
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '*' '*')
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x)
                         (Earley_core.Earley.char '$' '$')))
                   (Earley_core.Earley.empty
                      (fun g -> fun e -> `Fixpoint (e, g)))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '+' '+')
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x)
                         (Earley_core.Earley.char '$' '$')))
                   (Earley_core.Earley.empty
                      (fun g -> fun e -> `Fixpoint1 (e, g)))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '?' '?')
             (Earley_core.Earley.fsequence glr_opt_expr
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x)
                         (Earley_core.Earley.char '$' '$')))
                   (Earley_core.Earley.empty
                      (fun g -> fun e -> `Option (e, g)))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '$' '$')
             (Earley_core.Earley.empty `Greedy)])
    let _ =
      Earley_core.Earley.set_grammar glr_ident
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty (None, ("_", None)));
           Earley_core.Earley.fsequence (pattern_lvl (true, ConstrPat))
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char ':' ':')
                (Earley_core.Earley.empty
                   (fun p ->
                      match p.ppat_desc with
                      | Ppat_alias (p, { txt = id }) ->
                          ((Some true), (id, (Some p)))
                      | Ppat_var { txt = id } ->
                          ((Some (id <> "_")), (id, None))
                      | Ppat_any -> ((Some false), ("_", None))
                      | _ -> ((Some true), ("_", (Some p))))))])
    let _ =
      Earley_core.Earley.set_grammar glr_left_member
        (Earley_core.Earley.apply (fun f -> f [])
           (Earley_core.Earley.fixpoint1' (fun l -> l)
              (Earley_core.Earley.fsequence glr_ident
                 (Earley_core.Earley.fsequence glr_sequence
                    (Earley_core.Earley.fsequence glr_option
                       (Earley_core.Earley.empty
                          (fun opt ->
                             fun ((cst, s) as _default_0) ->
                               fun ((cst', id) as _default_1) ->
                                 `Normal
                                   (id,
                                     (from_opt cst' ((opt <> `Once) || cst)),
                                     s, opt))))))
              (fun x -> fun f -> fun l -> f (x :: l))))
    let _ =
      Earley_core.Earley.set_grammar glr_let
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty (fun x -> x));
           Earley_core.Earley.fsequence let_kw
             (Earley_core.Earley.fsequence rec_flag
                (Earley_core.Earley.fsequence let_binding
                   (Earley_core.Earley.fsequence in_kw
                      (Earley_core.Earley.fsequence glr_let
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun l ->
                                       fun _default_0 ->
                                         fun lbs ->
                                           fun r ->
                                             fun _default_1 ->
                                               fun x ->
                                                 Exp.let_ ~loc:_loc r lbs
                                                   (l x)))))))])
    let _ =
      Earley_core.Earley.set_grammar glr_cond
        (Earley_core.Earley.option None
           (Earley_core.Earley.apply (fun x -> Some x)
              (Earley_core.Earley.fsequence_ignore when_kw
                 (Earley_core.Earley.fsequence expression
                    (Earley_core.Earley.empty (fun e -> e))))))
    let _ =
      glr_action__set__grammar
        (fun alm ->
           Earley_core.Earley.alternatives
             [Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.empty ())
                (Earley_core.Earley.empty Default);
             Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.string "->>" "->>")
               (Earley_core.Earley.fsequence (glr_rule alm)
                  (Earley_core.Earley.empty
                     (fun r ->
                        let (a, b, c) = build_rule r in DepSeq (a, b, c))));
             Earley_core.Earley.fsequence arrow_re
               (Earley_core.Earley.fsequence
                  (if alm then expression else expression_lvl (Let, Seq))
                  (Earley_core.Earley.fsequence no_semi
                     (Earley_core.Earley.empty
                        (fun _default_0 ->
                           fun action -> fun _default_1 -> Normal action))))])
    let _ =
      glr_rule__set__grammar
        (fun alm ->
           Earley_core.Earley.fsequence glr_let
             (Earley_core.Earley.fsequence glr_left_member
                (Earley_core.Earley.fsequence glr_cond
                   (Earley_core.Earley.fsequence (glr_action alm)
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun action ->
                                    fun condition ->
                                      fun l ->
                                        fun def ->
                                          let l =
                                            fst
                                              (List.fold_right
                                                 (fun x ->
                                                    fun (res, i) ->
                                                      match x with
                                                      | `Normal
                                                          (("_", a), true, c,
                                                           d)
                                                          ->
                                                          (((`Normal
                                                               ((("_default_"
                                                                    ^
                                                                    (
                                                                    string_of_int
                                                                    i)), a),
                                                                 true, c, d,
                                                                 false)) ::
                                                            res), (i + 1))
                                                      | `Normal (id, b, c, d)
                                                          ->
                                                          let occur_loc_id =
                                                            ((fst id) <> "_")
                                                              &&
                                                              (occur
                                                                 ("_loc_" ^
                                                                    (
                                                                    fst id))
                                                                 action) in
                                                          (((`Normal
                                                               (id, b, c, d,
                                                                 occur_loc_id))
                                                            :: res), i)) l
                                                 ([], 0)) in
                                          let occur_loc = occur "_loc" action in
                                          (_loc, occur_loc, def, l,
                                            condition, action)))))))
    let _ =
      glr_at_rule__set__grammar
        (fun alm ->
           Earley_core.Earley.fsequence
             (Earley_core.Earley.option None
                (Earley_core.Earley.apply (fun x -> Some x)
                   (Earley_core.Earley.alternatives
                      [Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '[' '[')
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char '@' '@')
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string "unshared"
                                  "unshared")
                               (Earley_core.Earley.fsequence_ignore
                                  (Earley_core.Earley.char ']' ']')
                                  (Earley_core.Earley.empty ()))));
                      Earley_core.Earley.fsequence_ignore
                        (Earley_core.Earley.char '@' '@')
                        (Earley_core.Earley.empty ())])))
             (Earley_core.Earley.fsequence (glr_rule alm)
                (Earley_core.Earley.empty
                   (fun r -> fun a -> ((a <> None), r)))))
    let _ =
      Earley_core.Earley.set_grammar glr_rules
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.char '|' '|')))
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l)
                    (Earley_core.Earley.fsequence (glr_at_rule false)
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '|' '|')
                          (Earley_core.Earley.empty (fun r -> r))))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.fsequence (glr_at_rule true)
                 (Earley_core.Earley.empty
                    (fun r -> fun rs -> fun _default_0 -> r :: rs)))))
    let glr_binding = Earley_core.Earley.declare_grammar "glr_binding"
    let _ =
      Earley_core.Earley.set_grammar glr_binding
        (Earley_core.Earley.fsequence lident
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l) pattern
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option None
                    (Earley_core.Earley.apply (fun x -> Some x)
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '@' '@')
                          (Earley_core.Earley.fsequence pattern
                             (Earley_core.Earley.empty
                                (fun _default_0 -> _default_0))))))
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ':' ':')
                             (Earley_core.Earley.fsequence typexpr
                                (Earley_core.Earley.empty
                                   (fun _default_0 -> _default_0))))))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '=' '=')
                       (Earley_core.Earley.fsequence_position glr_rules
                          (Earley_core.Earley.empty
                             (fun str ->
                                fun pos ->
                                  fun str' ->
                                    fun pos' ->
                                      fun r ->
                                        let _loc_r = locate str pos str' pos' in
                                        fun ty ->
                                          fun prio ->
                                            fun args ->
                                              fun name ->
                                                `Parser
                                                  (name, args, prio, ty,
                                                    _loc_r, r)))))))))
    let glr_bindings = Earley_core.Earley.declare_grammar "glr_bindings"
    let _ =
      Earley_core.Earley.set_grammar glr_bindings
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence and_kw
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option []
                    (Earley_core.Earley.fsequence let_binding
                       (Earley_core.Earley.fsequence_ignore and_kw
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0)))))
                 (Earley_core.Earley.fsequence parser_kw
                    (Earley_core.Earley.fsequence glr_binding
                       (Earley_core.Earley.fsequence glr_bindings
                          (Earley_core.Earley.empty
                             (fun l ->
                                fun b ->
                                  fun _default_0 ->
                                    fun cs ->
                                      fun _default_1 ->
                                        (List.map (fun b -> `Caml b) cs) @ (b
                                          :: l)))))));
           Earley_core.Earley.fsequence
             (Earley_core.Earley.option []
                (Earley_core.Earley.fsequence_ignore and_kw
                   (Earley_core.Earley.fsequence let_binding
                      (Earley_core.Earley.empty
                         (fun _default_0 -> _default_0)))))
             (Earley_core.Earley.empty
                (fun cs -> List.map (fun b -> `Caml b) cs))])
    let extra_structure =
      let p =
        Earley_core.Earley.fsequence let_kw
          (Earley_core.Earley.fsequence parser_kw
             (Earley_core.Earley.fsequence glr_binding
                (Earley_core.Earley.fsequence glr_bindings
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun l ->
                                 fun b ->
                                   fun _default_0 ->
                                     fun _default_1 ->
                                       build_str_item _loc (b :: l)))))) in
      p :: extra_structure
    let extra_prefix_expressions =
      let p =
        Earley_core.Earley.fsequence
          (Earley_core.Earley.alternatives
             [Earley_core.Earley.fsequence_ignore function_kw
                (Earley_core.Earley.fsequence pattern
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char '@' '@')
                      (Earley_core.Earley.fsequence pattern
                         (Earley_core.Earley.fsequence_ignore arrow_re
                            (Earley_core.Earley.fsequence_ignore parser_kw
                               (Earley_core.Earley.empty
                                  (fun prio ->
                                     fun arg -> ([arg], (Some prio)))))))));
             Earley_core.Earley.fsequence_ignore parser_kw
               (Earley_core.Earley.empty ([], None));
             Earley_core.Earley.fsequence_ignore fun_kw
               (Earley_core.Earley.fsequence
                  (Earley_core.Earley.apply (fun f -> f [])
                     (Earley_core.Earley.fixpoint' (fun l -> l)
                        (pattern_lvl (false, AtomPat))
                        (fun x -> fun f -> fun l -> f (x :: l))))
                  (Earley_core.Earley.fsequence_ignore
                     (Earley_core.Earley.char '@' '@')
                     (Earley_core.Earley.fsequence pattern
                        (Earley_core.Earley.fsequence_ignore arrow_re
                           (Earley_core.Earley.fsequence_ignore parser_kw
                              (Earley_core.Earley.empty
                                 (fun prio -> fun args -> (args, (Some prio)))))))))])
          (Earley_core.Earley.fsequence_position glr_rules
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun r ->
                                   let _loc_r = locate str pos str' pos' in
                                   fun ((args, prio) as _default_0) ->
                                     let r =
                                       match prio with
                                       | None -> build_alternatives _loc_r r
                                       | Some prio ->
                                           build_prio_alternatives _loc_r
                                             prio r in
                                     List.fold_right
                                       (fun arg ->
                                          fun r ->
                                            {
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_fun
                                                   (Asttypes.Nolabel, None,
                                                     arg, r));
                                              Parsetree.pexp_loc = _loc;
                                              Parsetree.pexp_attributes = []
                                            }) args r))) in
      p :: extra_prefix_expressions
    let _ = add_reserved_id "parser"
  end
