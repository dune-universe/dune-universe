%{
    open Logic
    open AcgData.Environment
                        
    module F=Functions.Functions
               

    let echo ctx s = if F.echo ctx then Logs.app (fun m -> m "%s" s) else ()
    let wait ctx f = if F.should_wait ctx then ignore (f () )
    let svg ctx = F.svg ctx

    let new_loc (s,_) (_,e) = (s,e)

%}

%token EOII
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)> LOAD_DATA LOAD_SCRIPT LOAD_OBJECT
%token LOAD_HELP LIST SELECT UNSELECT HELP ANALYSE_HELP CHECK_HELP REALIZE_HELP
%token REALIZE_SHOW_HELP PARSE_HELP QUERY_HELP COMPOSE ADD_HELP AS WAIT EXIT
%token CREATE_SIG CREATE_LEX CREATE_HELP SAVE_HELP
%token <Logic.Abstract_syntax.Abstract_syntax.location> PRINT DONT TRACE 
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>ANALYSE
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>CHECK
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>REALIZE
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>REALIZE_SHOW
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>PARSE
%token <Logic.Abstract_syntax.Abstract_syntax.location>IDB
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>QUERY
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>ADD
%token <string>SEMICOLONN
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location)>IDENTT
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location*string)>SAVE


%start <Functions.Functions.context * AcgData.Environment.Environment.t -> Functions.Functions.context * AcgData.Environment.Environment.t> commands

                                                                                  %%

  commands:
| commands = command* EOII
                        {
                          fun (ctx,e) ->
                          List.fold_left
                            (fun (c,env) command ->
                              try
                                let () =
                                  if F.should_wait c then
                                    ignore (read_line ())
                                  else
                                    () in
                                command (c,env)
                              with
                              | Sys.Break
                                | End_of_file -> raise F.Stop)
                            (ctx,e)
                            commands
                        }

  command:
| EXIT l = SEMICOLONN {
               fun (ctx,e) ->
               let () = echo ctx l in
               let () = F.exit () in
               ctx,e}
                      
| WAIT l = SEMICOLONN {
               fun (ctx,e) ->
               let () = echo ctx l in
               let ctx' = F.wait ctx in
               (ctx',e)}
                      
                      
| WAIT HELP l = SEMICOLONN {
                    fun (ctx,e) ->
                    let () = echo ctx l in
                    let () = F.help (F.Help (Some F.Wait)) in
                    ctx,e}
                      
| DONT WAIT l = SEMICOLONN {
               fun (ctx,e) ->
               let () = echo ctx l in
               let ctx' = F.dont_wait ctx in (ctx',e)}
                           
| DONT WAIT HELP l = SEMICOLONN {
                    fun (ctx,e) ->
                    let () = echo ctx l in
                    let () = F.help (F.Help (Some F.Dont_wait)) in
                    ctx,e}
                           
| params = LOAD_DATA  {
      fun (ctx,e) ->
      let s,loc,l = params in
      let () = echo ctx l in
      F.load F.Data s (ctx,e)
      }

| params = LOAD_OBJECT  {
               fun (ctx,e) -> 
               let s,loc,l = params in
               let () = echo ctx l in 
	       F.load F.Object s (ctx,e)}
                        
| params = LOAD_SCRIPT  {
      fun (ctx,e) ->  
      let s,loc,l = params in
      let () = echo ctx l in
      let parse_script_fn filename (c,env) = (F.parse_script ctx) filename c env in
      F.load (F.Script parse_script_fn) s (ctx,e) }
                                   
| LIST l = SEMICOLONN {fun (ctx,e) ->
                       let () = echo ctx l in
                       let () = F.list e in
                       ctx, e}

| LIST HELP l = SEMICOLONN {
                    fun (ctx,e) ->
                    let () = echo ctx l in
                    let () = F.help (F.Help (Some F.List)) in
                    ctx,e}
                       
| SELECT id = IDENTT  l = SEMICOLONN {
                              fun (ctx,e) ->
                              let name,loc = id in
                              let () = echo ctx l in
                              ctx,F.select name loc e}
                                     
| SELECT HELP l = SEMICOLONN {
                      fun (ctx,e) ->
                      let () = echo ctx l in
                      let () = F.help (F.Help (Some F.Select)) in
                      ctx,e}

                                     
| UNSELECT l = SEMICOLONN { 
                   fun (ctx,e) ->
                   let () = echo ctx l in
                   ctx,F.unselect e}

| UNSELECT HELP l = SEMICOLONN {
                      fun (ctx,e) ->
                      let () = echo ctx l in
                      let () = F.help (F.Help (Some F.Unselect)) in
                      ctx,e}
                          
| t = TRACE l = SEMICOLONN { 
                fun (ctx,e) ->
                let () = echo ctx l in
                let () = F.trace t in
                ctx,e}

| TRACE HELP l = SEMICOLONN {
                     fun (ctx,e) ->
                     let () = echo ctx l in
                     let () = F.help (F.Help (Some F.Trace)) in
                     ctx,e}
                          
| d = DONT t = TRACE l = SEMICOLONN {
                     fun (ctx,e) ->
                     let () = echo ctx l in
                     let () = F.dont_trace (new_loc d t) in
                     ctx,e}
                               
| DONT TRACE HELP l = SEMICOLONN {
                     fun (ctx,e) ->
                     let () = echo ctx l in
                     let () = F.help (F.Help (Some F.Dont_trace)) in
                     ctx,e}
                          
| name = IDENTT? p = PRINT l = SEMICOLONN {
                                   fun (ctx,e) ->
                                   let () = echo ctx l in
                                   let () = 
                                     match name with
                                     | None -> F.print e p
                                     | Some (n,l) -> F.print ~name:n e l in
                                   ctx,e}

| IDENTT? PRINT HELP l = SEMICOLONN {
                            fun (ctx,e) ->
                            let () = echo ctx l in
                            let () = F.help (F.Help (Some F.Print)) in
                            ctx,e}
                          
| params = ANALYSE   {
               fun (ctx,e) ->
               let t,l,line = params in
               let () = echo ctx line in
	       let () = F.analyse e t l in
               ctx, e}

| names = IDENTT+ params = ANALYSE   {
                               fun (ctx,e) ->
                               let t,l,line = params in
                               let () = echo ctx line in
                               let () = F.analyse  ~names e t l in
                               ctx, e}
| params = CHECK  {
               fun (ctx,e) ->
               let t,l,line = params in
               let () = echo ctx line in 
               let () = F.check e t l in
               ctx, e}

| names = IDENTT+ params = CHECK  {
                               fun (ctx,e) ->
                               let t,l,line = params in
                               let () = echo ctx line in 
                               let () = F.check  ~names e t l in
                               ctx, e}
| params = REALIZE   {
               fun (ctx,e) ->
               let t,l,line = params in
               let () = echo ctx line in  
               let () = F.realize ?svg_output:(F.svg ctx) e t l in
               ctx, e}

| names = IDENTT+ params = REALIZE   {
                               fun (ctx,e) ->
                               let t,l,line = params in
                               let () = echo ctx line in  
                               let () = F.realize  ~names ?svg_output:(F.svg ctx) e t l in
                               ctx, e}
| params = REALIZE_SHOW   { 
               fun (ctx,e) ->
               let t,l,line = params in
               let () = echo ctx line in  
               let () = F.realize_show e t l in
               ctx,e}

| names = IDENTT+ params = REALIZE_SHOW   { 
                               fun (ctx,e) ->
                               let t,l,line = params in
                               let () = echo ctx line in  
                               let () = F.realize_show  ~names e t l in
                               ctx,e}
| params = PARSE   {
               fun (ctx,e) ->
               let t,l,line = params in
               let () = echo ctx line in
               let () =  F.parse  e t l in
               ctx,e}
| name = IDENTT params = PARSE   {
                             fun (ctx,e) ->
                             let t,l,line = params in
                             let () = echo ctx line in
                             let n,lex_loc = name in
                             let () = F.parse ~name:n e t lex_loc in
                             ctx,e}
| params = QUERY  {
               fun (ctx,e) ->
               let t,l,line = params in
               let () = echo ctx line in
               let () = F.query  e t l in
               ctx,e}
| name = IDENTT params = QUERY  {
                             fun (ctx,e) ->
                             let t,l,line = params in
                             let () = echo ctx line in
                             let n,lex_loc = name in
                             let () = F.query ~name:n e t lex_loc in
                             ctx,e}
                                
| name = IDENTT? p = IDB l = SEMICOLONN {
                                 fun (ctx,e) ->
                                 let () = echo ctx l in
                                 let () = 
                                   match name with
                                   | None -> F.idb e p
                                   | Some (n,l) -> F.idb ~name:n e l in
                                 ctx,e}

| IDENTT? IDB HELP l = SEMICOLONN {
                            fun (ctx,e) ->
                            let () = echo ctx l in
                            let () = F.help (F.Help (Some F.Idb)) in
                            ctx,e}

| params = ADD   {
               fun (ctx,e) ->
               let t,l,line = params in
               let () = echo ctx line in
               ctx, F.add e t l}
| names = IDENTT+ params = ADD   {
                               fun (ctx,e) ->
                               let t,l,line = params in
                               let () = echo ctx line in
                               ctx, F.add  ~names e t l}
                                 
| COMPOSE n1 = IDENTT n2 = IDENTT AS n3 = IDENTT l = SEMICOLONN {
                                                         fun (ctx,e) ->
                                                         let () = echo ctx l in
                                                         ctx,F.compose n1 n2 n3 e}
| COMPOSE HELP l = SEMICOLONN {
                       fun (ctx,e) ->
                       let () = echo ctx l in
                       let () = F.help (F.Help (Some F.Compose)) in
                       ctx,e}
                              
| HELP l = SEMICOLONN {
               fun (ctx,e) ->
               let () = echo ctx l in
               let () = F.help (F.Help None) in
               ctx,e}
                      
| HELP HELP l = SEMICOLONN {
                    fun (ctx,e) ->
                    let () = echo ctx l in
                    let () = F.help (F.Help (Some (F.Help None))) in
                    ctx,e}
                                         
| LOAD_HELP l = SEMICOLONN {
                    fun (ctx,e) ->
                    let () = echo ctx l in
                    let () = F.help (F.Help (Some F.Load)) in
                    ctx,e}
                           
                           
                           
| ANALYSE_HELP l = SEMICOLONN {
                       fun (ctx,e) ->
                       let () = echo ctx l in
                       let () = F.help (F.Help (Some F.Analyse)) in
                       ctx,e}
                              
| REALIZE_HELP l = SEMICOLONN {
                       fun (ctx,e) ->
                       let () = echo ctx l in
                       let () = F.help (F.Help (Some F.Realize)) in ctx,e}
                              
| REALIZE_SHOW_HELP l = SEMICOLONN {
                            fun (ctx,e) ->
                            let () = echo ctx l in
                            let () = F.help (F.Help (Some F.RealizeShow)) in ctx,e}
                                   
| CHECK_HELP l = SEMICOLONN {
                     fun (ctx,e) ->
                     let () = echo ctx l in
                     let () = F.help (F.Help (Some F.Check)) in ctx,e}
                            
| PARSE_HELP l = SEMICOLONN {
                     fun (ctx,e) ->
                     let () = echo ctx l in
                     let () = F.help (F.Help (Some F.Parse)) in ctx,e}
                            
| QUERY_HELP l = SEMICOLONN {
                     fun (ctx,e) ->
                     let () = echo ctx l in
                     let () = F.help (F.Help (Some F.Query)) in ctx,e}
                            
| SAVE_HELP l = SEMICOLONN {
                    fun (ctx,e) ->
                    let () = echo ctx l in
                    let () = F.help (F.Help (Some F.Save)) in ctx,e}
                           
| ADD_HELP l = SEMICOLONN {
                   fun (ctx,e) ->
                   let () = echo ctx l in
                   let () = F.help (F.Help (Some F.Add)) in ctx,e}
                          
| CREATE_HELP l = SEMICOLONN {
                      fun (ctx,e) ->
                      let () = echo ctx l in
                      let () = F.help (F.Help (Some F.Create)) in ctx,e}
                             
                             
| CREATE_SIG n = IDENTT l = SEMICOLONN {
                                fun (ctx,e) ->
                                let () = echo ctx l in
                                ctx, F.create_sig n e}
                                       
| CREATE_LEX n = IDENTT n1 = IDENTT n2 = IDENTT  l = SEMICOLONN {
                                                         fun (ctx,e) ->
                                                         let () = echo ctx l in
                                                         ctx,F.create_lex ~abs:n1 ~obj:n2 n e}
                                                                
| params = SAVE   { 
               fun (ctx,e) ->
               let filename,l,line = params in
               let () = echo ctx line in
               let () =  F.save filename e l in
               ctx,e}
                                                                
| names = IDENTT+ params = SAVE   { 
                               fun (ctx,e) ->
                               let filename,l,line = params in
                               let () = echo ctx line in
                               let () =  F.save ~names filename e l in
                               ctx,e}
                                  
