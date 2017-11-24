# SDK and command line interface for Watson Conversation Service

wcs-ocaml is a source development kit in OCaml and command line interface for
[Watson Conversation Service (WCS)](https://www.ibm.com/watson/services/conversation/). It allows to program chat bots in OCaml.

* `wcs-lib` provides a framework to write WCS programs, called
  workspaces.
* `wcs-api` offers an OCaml binding to the
  [service API](https://www.ibm.com/watson/developercloud/conversation/api/v1/)
  and a generic client application.
* `wcs` is a command line tool that interact with the service.

The documentation of the packages is defined in the interface of the modules
and  available online:
* [`wcs-lib`](wcs-lib)
* [`wcs-api`](wcs-api)

The `wcs` command line tool allows to do operations like listing the workspaces,
uploading or updating workspaces. The full documentation if avaible with the
command `wcs -help` or [online](https://ibm.github.io/wcs-ocaml/wcs.html).

## Quick install with Opam

The easiest way to install wcs-ocaml is through the Opam package manager for OCaml.
Instructions to install Opam on you system can be found on the website:
<http://opam.ocaml.org/doc/Install.html>.

Then you can install wcs-ocaml with the following command:
```
opam install wcs
```

This will install the three main packages:
- `wcs-lib`,
- `wcs-api`, and,
- `wcs`

Alternatively, you can only install the WCS SDK:
```
opam install wcs-lib
```

or the WCS API (which will also install `wcs-lib`):
```
opam install wcs-api
```

# Example

In order to illustrate the use of wcs-ocaml, we are going to program a
bot that tells a knock knock joke.

Let's start with a dialog node that says `"Knock knock"`:

```ocaml
let knock =
  Wcs.dialog_node "Knock"
    ~conditions: "true"
    ~text: "Knock knock"
    ()
```

The function
[`Wcs.dialog_node`](https://ibm.github.io/wcs-ocaml/wcs-lib/Wcs/index.html#val-dialog_node)
creates a value of type
[`Wcs_t.dialog_node`](https://ibm.github.io/wcs-ocaml/wcs-lib/Wcs_t/index.html#type-dialog_node)
that corresponds to a JSON object of type
[`DialogNode`](https://www.ibm.com/watson/developercloud/conversation/api/v1/)
in WCS.

The user is expected to ask _who is there?_. To capture this intent
without looking for an exact match, we can define a WCS intent using
multiple examples to train the NLU:

```ocaml
let who_intent =
  Wcs.intent "Who"
    ~examples: [
      "Who's there?";
      "Who is there?";
      "Who are you?";
    ]
    ()
```

We can now define the next step of the dialog, answering the question
_who is there?_:

```ocaml
let whoisthere =
  Wcs.dialog_node "WhoIsThere"
    ~conditions_spel: (Spel.intent who_intent)
    ~text: "Broken Pencil"
    ~parent: knock
    ()
```

The condition is not a string but an expression written using the
embedding of the Spel expression language (used by WCS) in OCaml.

We now expect the user to repeat the name of the character mentioned
by the bot.  To test that the user input matches the same character,
we define an entity `char_entity` containing the name and a list of
synonyms.

```ocaml
let char_entity =
  Wcs.entity "Character"
    ~values: [ "Broken Pencil", ["Dammaged Pen"; "Fractured Pencil"] ]
    ()
```

The bot terminates the joke if the input given by the user matches the
name of the character. Setting a `return` field in the context triggers
the termination of the bot.

```ocaml
let answer =
  Wcs.dialog_node "Answer"
    ~conditions_spel: (Spel.entity char_entity ())
    ~text: "Nevermind it's pointless"
    ~parent: whoisthere
    ~context: (Context.return (Json.bool true))
    ()
```



If the user doesn't gives the name of the character, the bot can help
with a generic answer using a fallback node:

```ocaml
let fallback =
  Wcs.dialog_node "Fallback"
    ~conditions_spel: Spel.anything_else
    ~text: "You should repeat my name!"
    ~previous_sibling: answer
    ~next_step: (whoisthere, Wcs_t.Goto_body)
    ()
```

We can now build the entire workspace containing all the dialog nodes,
entities, and intents:

```ocaml
let ws_knockknock =
  Wcs.workspace "Knock Knock"
    ~entities: [ char_entity ]
    ~intents: [ who_intent ]
    ~dialog_nodes: [ knock; whoisthere; answer; fallback; ]
    ()
```

It is possible to print this workspace:

```ocaml
let () = print_endline (Wcs_pretty.workspace ws_knockknock)
```

It is also possible to directly deploy the workspace on WCS. The
deployment requires the service credentials:

```ocaml
let wcs_cred = Wcs_bot.get_credential None
```

The function
[Wcs_bot.get_credential](https://ibm.github.io/wcs-ocaml/wcs-api/Wcs_bot/index.html#val-get_credential)
retrieves the path stored in the environment variable `WCS_CRED` to
find a file containing the service credentials in the following
format:

```js
{
  "url": "https://gateway.watsonplatform.net/conversation/api",
  "password": "PASSWORD",
  "username": "USERNAME"
}
```



We can now deploy the workspace on WCS:

```ocaml
let create_rsp = Wcs_api.create_workspace wcs_cred ws_knockknock
```

Finally, we can try the bot with the function
[Wcs_bot.exec](https://ibm.github.io/wcs-ocaml/wcs-api/Wcs_bot/index.html#val-exec)
providing the credentials and the workspace identifier that has just
been created:

```ocaml
let _ =
  begin match create_rsp with
  | { Wcs_t.crea_rsp_workspace_id = Some id } ->
    Wcs_bot.exec wcs_cred id Json.null ""
  | _  -> failwith "Deployment error"
  end
```

To compile this program, we need to link the libraries `wcs-lib` and
`wcs-api`. Using `ocamlfind` the command is:

```
ocamlfind ocamlc -linkpkg -package wcs-lib -package wcs-api knockknock.ml
```
