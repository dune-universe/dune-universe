# rulebot
Dialog interface for authoring of events rules

## Building
### Prerequistes

To build from source, you will need:
- OCaml 4.04 or later (http://ocaml.org/) along with the following library:
  - wcs-ocaml, the Watson Conversation Service SDK (https://ibm.github.io/wcs-ocaml/)
- ReactiveML 1.09.05 or later (http://reactiveml.org)
```
opam install wcs
opam install rml
```

### Compiling

To compile, do:

```
make
```

This should create an executable called `src/r_rulebot`.

## Running

To run a conversation flow, you should do:
```
src/rulebot -wcs-cred wcs_credential.json
```
where `wcs_credential.json` is a file containing your Watson Conversation credentials as follows:
```
{
  "url": "https://gateway.watsonplatform.net/conversation/api",
  "password": "...",
  "username": "..."
}
```
If the path to the credentials file is given in the `$WCS_CRED` environment variable, it is not necessary to provide the `-wcs-cred` option.

This command automatically deploys the workspaces on Watson Conversation Service.

If you don't want to deploy the workspaces but use some already deployed, you can use the option `-ws-config` to indicate the file containing the ids of the workspaces. The file should have the following format:
```
{
  "ws_dispatch_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
  "ws_when_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
  "ws_cond_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
  "ws_cond_continue_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
  "ws_then_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
  "ws_expr_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
  "ws_actn_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
  "ws_accept_id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
}
```

You can notice that the configuration file is printed on the standard output when you launch the bot. If some of the fields of the workspace configuration file are not specified, the bot regenerates and deploy them.

Instead of using workspaces already deployed or regenerate workspaces, it is also possible to deploy workspaces given as JSON files. If the workspace configuration file contains the following fields, the files specified are going to be uploaded:
```
{
  "ws_select_example": "./workspaces/rulebot-select-example.json",
  "ws_dispatch": "./workspaces/rulebot-dispatch.json",
  "ws_when": "./workspaces/rulebot-when.json",
  "ws_cond": "./workspaces/rulebot-cond.json",
  "ws_then": "./workspaces/rulebot-then-ml.json",
  "ws_expr": "./workspaces/rulebot-expr-ml.json",
  "ws_actn": "./workspaces/rulebot-actn-ml.json",
  "ws_accept": "./workspaces/rulebot-accept-ml.json"
}
```
The workspaces can be generated as JSON files using the option `ws-gen`.

If the option `-ws-update` is provided, the workspaces ids given in the workspace configuration file are used to redeploy the workspaces.

Finally, the workspaces specified in the workspace configuration file can be removed from WCS using the `-ws-delete` options:
```
src/r_rulebot -wcs-cred wcs_credential.json -ws-config ws-config.json -ws-delete
```

The documentation of all options is available with:
```
src/r_rulebot -help
```
