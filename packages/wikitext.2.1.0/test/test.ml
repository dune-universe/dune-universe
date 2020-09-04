open OUnit2
open Wikitext

let assert_equal expected input =
  let lexbuf = Lexing.from_string input in
  doc_from_lexbuf lexbuf
  |> Mapper.set_toc
  |> Mapper.set_links
  |> Mapper.normalize
  |> assert_equal ~printer:Test_show.printer expected

let quote_fail1 _ctx =
  assert_equal
    [ Paragraph [ String "test : 'a'" ]]
    "test : 'a'"

let bold_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem "
                ; Bold [ String "ipsum dolor" ]
                ; String " sit amet"
                ]
    ]
    "Lorem '''ipsum dolor''' sit amet"

let bold_2 _ctx =
  assert_equal
    [ Paragraph [ Bold [ String "bold text" ]
                ; String " at begining of a line"
                ]
    ]
    "'''bold text''' at begining of a line"

let bold_3 _ctx =
  assert_equal
    [ Paragraph [ String "bold text at "
                ; Bold [ String "end of line" ]
                ]
    ]
    "bold text at '''end of line'''"


let italic_1 _ctx =
  assert_equal
    [ Paragraph [ Italic [ String "italic text" ]
                ; String " at begining of a line"
        ]
    ]
    "''italic text'' at begining of a line"

let italic_2 _ctx =
  assert_equal
    [ Paragraph [ String "Some "
                ; Italic [ String "italic text" ]
                ; String " in the middle of the line"
                ]
    ]
    "Some ''italic text'' in the middle of the line"

let italic_3 _ctx =
  assert_equal
    [ Paragraph [ String "italic text at "
                ; Italic [ String "end of line" ]
                ]
    ]
    "italic text at ''end of line''"

let italic_4 _ctx =
  assert_equal
    [ Paragraph [Italic [ String "italic text on a complete line" ] ] ]
    "''italic text on a complete line''"

let bolditalic_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem "
                ; Bold [ Italic [ String "ipsum" ] ]
                ; String "."
                ]
    ]
    "Lorem '''''ipsum'''''."

let paragraph_1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem ipsum"] ;
      Paragraph [ String "dolores sit amet"]
    ]
    "Lorem ipsum  \n  \ndolores sit amet  "

let paragraph_2 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem ipsum <= dolores sit amet" ] ]
    "Lorem ipsum <= dolores sit amet"

let paragraph_3 _ctx =
  assert_equal
    [ Paragraph [ String "x <=" ]
    ; Header (1, [ String "Title" ] ) ]
    "x <=\n= Title ="

let paragraph_4 _ctx =
  assert_equal
    [ Paragraph [ String "<=x" ] ]
    "<=x"

let space_1 _ctx =
  assert_equal
    [ Paragraph [ String "italic text at"
                ; Italic [ String " end of line" ]
                ]
    ]
    "italic text at'' end of line''"

let space_2 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem ipsum\ndolores"
                ]
    ]
    "Lorem ipsum\ndolores"

let link_1 _ctx =
  assert_equal
    [ Paragraph [ String "This is an <a href=\"www.test.com\">external link</a>." ] ]
    "This is an [www.test.com external link]."

let link_2 _ctx =
  assert_equal
    [ Paragraph [ String "This is a <a href=\"PageName\">link</a>." ]
    ]
    "This is a [[PageName|link]]."

let list_1 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ] ]
           ; [ Paragraph [ String "2" ] ]
           ; [ Paragraph [ String "3" ] ]
        ]
    ]
    "* 1\n\
     * 2\n\
     * 3"

let list_2 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ]
             ; List [ [ Paragraph [ String "1.1" ] ]
                    ; [ Paragraph [ String "1.2" ]
                      ; List [ [ Paragraph [ String "1.2.1" ] ] ]
                      ]
                 ]
             ]
           ; [ Paragraph [ String "2" ] ]
        ]
    ]
    "* 1\n\
     ** 1.1\n\
     ** 1.2\n\
     *** 1.2.1\n\
     * 2"

let list_3 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ]]]
    ]
    "* 1"

let list_4 _ctx =
  assert_equal
    [ List [[ List [ [ Paragraph [ String "1" ]]]]]
    ]
    "** 1"

let list_5 _ctx =
  assert_equal
    [ NumList [[ Paragraph [ String "1"]]]]
    "# 1"

let list_6 _ctx =
  assert_equal
    [ NumList [[ NumList [ [ Paragraph [ String "1" ]]]]]
    ]
    "## 1"

let list_7 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ]
             ; List [ [ List [ [ Paragraph [ String "1.1.1" ] ] ] ] ] ] ] ]
    "* 1\n\
     *** 1.1.1"

let list_8 _ctx =
  assert_equal
    [ List [ [ Paragraph [ String "1" ]
             ; List [ [ List [ [ Paragraph [ String "1.1.1"] ] ] ]
             ; [ Paragraph [ String "1.2" ] ] ] ]
           ]
    ]
    "* 1\n\
     *** 1.1.1\n\
     ** 1.2"

let list_9 _ctx =
  assert_equal
    [ List [ [ List [[Paragraph [ String "1"]
              ; List [[Paragraph [String "2"] ]] ]]]]
    ]
    "** 1\n\
     *** 2"

let list_mixed_1 _ctx =
  assert_equal
    [ NumList [[ Paragraph [ String "1" ]
                ; List [[ Paragraph [String "1.1"] ]] ]
              ; [ Paragraph [ String "2" ] ]
              ]
    ]
    "# 1\n\
     **1.1\n\
     # 2"

let list_mixed_2 _ctx =
  assert_equal
    [ List [[ Paragraph [ String "1" ]
                ; NumList [[ Paragraph [String "1.1"] ]] ]
              ; [ Paragraph [ String "2" ] ]
              ]
    ]
    "* 1\n\
     ##1.1\n\
     * 2"

let list_mixed_3 _ctx = (* no warning should be written on STDERR *)
  assert_equal
    [
        List [[ Paragraph [ String "1" ]]]
      ; NumList [[ Paragraph [ String "2" ]]]
      ; List [[ Paragraph [ String "3" ]]]
    ]
    "* 1\n\
     # 2\n\
     * 3"

let list_mixed_4 _ctx = (* no warning should be written on STDERR *)
  assert_equal
    [
        NumList [[ Paragraph [ String "1" ]]]
      ; List [[ Paragraph [ String "2" ]]]
      ; NumList [[ Paragraph [ String "3" ]]]
    ]
    "# 1\n\
     * 2\n\
     # 3"

let list_mixed_5 _ctx = (* no warning should be written on STDERR *)
  assert_equal
    [
        List [[ Paragraph [ String "1"] ;
              NumList [[ Paragraph [ String "1.1" ]]]
             ]]
      ; NumList [[ Paragraph [ String "3" ]]]
    ]
    "* 1\n\
     ## 1.1\n\
     # 3"

let list_mixed_warning_1 _ctx = (* a warning should be written on STDERR for this test *)
  assert_equal
    [
        List  [[ Paragraph [String "1"]
                ; NumList [ [ Paragraph [ String "1.1" ]]
                            ; [ Paragraph [ String "1.2" ]] ]
              ]]
    ]
    "* 1\n\
     ## 1.1\n\
     ** 1.2"

let def_1 _ctx =
  assert_equal
    [ DefList [ ([String "t1:term"], [])
              ] ]
    ";t1:term"

let def_2 _ctx =
  assert_equal
    [ DefList [ ([String "t1"], [
                    Paragraph [ String "d1" ]
                  ])
              ] ]
    ";t1\n\
     :d1"

let def_3 _ctx =
  assert_equal
    [ DefList [ ([String "t1"],
    [ Paragraph [ String "d1" ]
                  ; Paragraph [ String "d2" ]
                  ])
              ] ]
    ";t1\n\
     :d1\n\
     :d2"

let def_4 _ctx =
  assert_equal
    [ DefList [ ([], [ Paragraph [ String "d1" ] ])
              ] ]
    ":d1"

let def_5 _ctx =
  assert_equal
    [ DefList [ ([String "t1"]), []]
    ]
    ";t1"

let def_6 _ctx =
  assert_equal
    [ DefList [ ([String "t1:d1"], [ Paragraph [ String "d2" ]])]
    ]
    ";t1:d1\n\
     :d2"

let def_7 _ctx =
  assert_equal
    [ DefList [ ([String "tt:2"], [
                DefList [ ( [String "t1:1"],
                            [ Paragraph [ String "dd:d1.1" ]])
                        ]]
                )
              ]
    ]
    ";tt:2\n\
     :;t1:1\n\
     ::dd:d1.1"

let table_empty_cell_1 _ctx =
  assert_equal
    [ Table ([String "Titre"], [  [ TableItem [String ""]
                                                ; TableHead [String "Titre A"]
                                                ; TableHead [String "Titre B"]
                                                ; TableHead [String "Titre C"]
                                                ]
                                              ; [ TableHead [String "Titre 1"]
                                                ; TableItem [String "Data 1A"]
                                                ; TableItem [String "Data 1B"]
                                                ; TableItem [String "Data 1C"]
                                                ]
                                              ; [ TableHead [String "Titre 2"]
                                                ; TableItem [String "Data 2A"]
                                                ; TableItem [String "Data 2B"]
                                                ; TableItem [String "Data 2C"]
                                                ]
                                             ])
    ]
    "{|\n\
      |+ Titre\n\
      |---------------------------------\n\
      |\n\
      ! Titre A\n\
      ! Titre B\n\
      ! Titre C\n\
      |-------------------------------------------\n\
      ! Titre 1\n\
      | Data 1A\n\
      | Data 1B\n\
      | Data 1C\n\
      |-------------------------------------------\n\
      ! Titre 2\n\
      | Data 2A\n\
      | Data 2B\n\
      | Data 2C\n\
      |}"

let table_empty_cell_2 _ctx =
  assert_equal
    [ Table ([String "Titre"], [  [ TableItem []
                                                ; TableHead [String "Titre A"]
                                                ; TableHead [String "Titre B"]
                                                ; TableHead [String "Titre C"]
                                                ]
                                              ; [ TableHead [String "Titre 1"]
                                                ; TableItem [String "Data 1A"]
                                                ; TableItem [String "Data 1B"]
                                                ; TableItem [String "Data 1C"]
                                                ]
                                              ; [ TableHead [String "Titre 2"]
                                                ; TableItem [String "Data 2A"]
                                                ; TableItem [String "Data 2B"]
                                                ; TableItem [String "Data 2C"]
                                                ]
                                             ])
    ]
    "{|\n\
      |+ Titre\n\
      |------------------------------------\n\
      | !! Titre A !! Titre B !! Titre C\n\
      |-------------------------------------\n\
      ! Titre 1\n\
      | Data 1A || Data 1B || Data 1C\n\
      |---------------------------------------\n\
      ! Titre 2\n\
      | Data 2A || Data 2B || Data 2C\n\
      |}"

let table_no_title _ctx =
  assert_equal
    [ Table ([], [  [ TableItem []
                                                ; TableHead [String "Titre A"]
                                                ; TableHead [String "Titre B"]
                                                ; TableHead [String "Titre C"]
                                                ]
                                              ; [ TableHead [String "Titre 1"]
                                                ; TableItem [String "Data 1A"]
                                                ; TableItem [String "Data 1B"]
                                                ; TableItem [String "Data 1C"]
                                                ]
                                              ; [ TableHead [String "Titre 2"]
                                                ; TableItem [String "Data 2A"]
                                                ; TableItem [String "Data 2B"]
                                                ; TableHead [String "Data 2C"]
                                                ]
                                             ])
    ]
    "{|\n\
      |---------------------------------------------------------\n\
      | !! Titre A\n\
      !Titre B !! Titre C\n\
      |-------------------------------\n\
      ! Titre 1\n\
      | Data 1A || Data 1B || Data 1C\n\
      |---------------------------------------------------\n\
      ! Titre 2\n\
      | Data 2A || Data 2B !! Data 2C\n\
      |}"

let table_no_first_row _ctx =
  assert_equal
    [ Table ([String "Table Title"], [  [ TableItem []
                                                ; TableHead [String "Titre A"]
                                                ; TableHead [String "Titre B"]
                                                ; TableHead [String "Titre C"]
                                                ]
                                              ; [ TableHead [String "Titre 1"]
                                                ; TableItem [String "Data 1A"]
                                                ; TableItem [String "Data 1B"]
                                                ; TableItem [String "Data 1C"]
                                                ]
                                              ; [ TableHead [String "Titre 2"]
                                                ; TableItem [String "Data 2A"]
                                                ; TableItem [String "Data 2B"]
                                                ; TableHead [String "Data 2C"]
                                                ]
                                             ])
    ]
    "{|\n\
      |+ Table Title\n\
      | !! Titre A\n\
      !Titre B !! Titre C\n\
      |-------------------------------\n\
      ! Titre 1\n\
      | Data 1A || Data 1B || Data 1C\n\
      |---------------------------------------------------\n\
      ! Titre 2\n\
      | Data 2A || Data 2B !! Data 2C\n\
      |}"

let table_no_title_no_first_row _ctx =
  assert_equal
    [ Table ([], [  [ TableItem []
                                                ; TableHead [String "Titre A"]
                                                ; TableHead [String "Titre B"]
                                                ; TableHead [String "Titre C"]
                                                ]
                                              ; [ TableHead [String "Titre 1"]
                                                ; TableItem [String "Data 1A"]
                                                ; TableItem [String "Data 1B"]
                                                ; TableItem [String "Data 1C"]
                                                ]
                                              ; [ TableHead [String "Titre 2"]
                                                ; TableItem [String "Data 2A"]
                                                ; TableItem [String "Data 2B"]
                                                ; TableHead [String "Data 2C"]
                                                ]
                                             ])
    ]
    "{|\n\
      | !! Titre A\n\
      !Titre B !! Titre C\n\
      |-------------------------------\n\
      ! Titre 1\n\
      | Data 1A || Data 1B || Data 1C\n\
      |---------------------------------------------------\n\
      ! Titre 2\n\
      | Data 2A || Data 2B !! Data 2C\n\
      |}"

let special_chars1 _ctx =
  assert_equal
    [ Paragraph [ String "Lorem || ipsum"
                ]
    ]
    "Lorem || ipsum"

let special_chars2 _ctx =
  assert_equal
    [ Paragraph [String "Lorem : ipsum"]
    ]
    "Lorem : ipsum"

let special_chars3 _ctx =
  assert_equal
    [ Paragraph [String "Lorem !! ipsum"]
    ]
    "Lorem !! ipsum"

let special_chars4 _ctx =
  assert_equal
    [ Paragraph [String "Lorem == ipsum"]
    ]
    "Lorem == ipsum"

let nowiki_1 _ctx =
  assert_equal
    [ NoWikiBlock "[::;] d]{''' ''''' ''}----||"
    ]
    "<nowiki>[::;] d]{''' ''''' ''}----||</nowiki>"

let nowiki_2 _ctx =
  assert_equal
    [ Paragraph [ NoWiki "''test''"
                ; String " "
                ; Bold [ String "gras" ]
                ; String " "
                ; NoWiki "'''pas gras'''"
                ]
    ]
    "<nowiki>''test''</nowiki> '''gras''' <nowiki>'''pas gras'''</nowiki>"

let normalize_1 _ctx =
  assert_equal
    [ Paragraph [ String "test " ; Bold [ String "test : test"] ; String " test : test" ]
    ]
    "test '''test : test''' test : test"

let () =
  run_test_tt_main
    ("wktxt" >::: [ "quote_fail1" >:: quote_fail1
                  ; "bold_1" >:: bold_1
                  ; "bold_2" >:: bold_2
                  ; "bold_3" >:: bold_3
                  ; "italic_1" >:: italic_1
                  ; "italic_2" >:: italic_2
                  ; "italic_3" >:: italic_3
                  ; "italic_4" >:: italic_4
                  ; "bolditalic_1" >:: bolditalic_1
                  ; "paragraph_1" >:: paragraph_1
                  ; "paragraph_2" >:: paragraph_2
                  ; "paragraph_3" >:: paragraph_3
                  ; "paragraph_4" >:: paragraph_4
                  ; "space_1" >:: space_1
                  ; "space_2" >:: space_2
                  ; "link_1" >:: link_1
                  ; "link_2" >:: link_2
                  ; "list_3" >:: list_3
                  ; "list_4" >:: list_4
                  ; "list_5" >:: list_5
                  ; "list_6" >:: list_6
                  ; "list_7" >:: list_7
                  ; "list_8" >:: list_8
                  ; "list_9" >:: list_9
                  ; "list_mixed_1" >:: list_mixed_1
                  ; "list_mixed_2" >:: list_mixed_2
                  ; "list_mixed_3" >:: list_mixed_3
                  ; "list_mixed_4" >:: list_mixed_4
                  ; "list_mixed_5" >:: list_mixed_5
                  ; "list_mixed_warning_1" >:: list_mixed_warning_1
                  ; "def_1" >:: def_1
                  ; "def_2" >:: def_2
                  ; "def_3" >:: def_3
                  ; "def_4" >:: def_4
                  ; "def_5" >:: def_5
                  ; "def_6" >:: def_6
                  ; "def_7" >:: def_7
                  ; "table_empty_cell_1" >:: table_empty_cell_1
                  ; "table_empty_cell_2" >:: table_empty_cell_2
                  ; "table_no_title" >:: table_no_title
                  ; "table_no_first_row" >:: table_no_first_row
                  ; "table_no_title_no_first_row" >:: table_no_title_no_first_row
                  ; "special_chars1" >:: special_chars1
                  ; "special_chars2" >:: special_chars2
                  ; "special_chars3" >:: special_chars3
                  ; "special_chars4" >:: special_chars4
                  ; "nowiki_1" >:: nowiki_1
                  ; "nowiki_2" >:: nowiki_2
                  ; "normalize_1" >:: normalize_1
                  ])
