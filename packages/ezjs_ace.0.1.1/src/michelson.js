ace.define('ace/mode/michelson', function(require, exports, module) {

var oop = require("ace/lib/oop");
var TextMode = require("ace/mode/text").Mode;
var MichelsonHighlightRules = require("ace/mode/michelson_highlight_rules").MichelsonHighlightRules;

var Mode = function() {
    this.HighlightRules = MichelsonHighlightRules;
};
oop.inherits(Mode, TextMode);

// (function() {
//     // Extra logic goes here. (see below)
// }).call(Mode.prototype);

exports.Mode = Mode;
});

ace.define('ace/mode/michelson_highlight_rules', function(require, exports, module) {

var oop = require("ace/lib/oop");
var TextHighlightRules = require("ace/mode/text_highlight_rules").TextHighlightRules;

var MichelsonHighlightRules = function() {

    this.$rules = { start:
       [ { token: 'keyword.michelson',
           regex: '\\b(parameter|storage|code|return)\\b'},
         { token: 'variable.michelson',
           regex: '\\b(unit|timestamp|tez|int|nat|bool|key|signature|string|pair|or|contract|lambda|map|set|list|option)\\b'},
         { token: 'keyword.operator.michelson',
           regex: '\\b(DROP|SWAP|IF|IF_NONE|IF_LEFT|IF_CONS|NOW|PAIR|BALANCE|SUB|ADD|MUL|NEQ|EQ|LT|LE|GT|GE|GET|UPDATE|MEM|SOME|MANAGER|SOURCE|MAP|OR|LAMBDA|REDUCE|COMPARE|CMP(EQ|NEQ|LT|GT|LE|GE)|IFCMP(EQ|NEQ|LT|GT|LE|GE)|FAIL|UNIT|TRANSFER_TOKENS|PUSH|H|CHECK_SIGNATURE|CONCAT|EDIV|EXEC|MOD|DIV|AMOUNT|NIL|EMPTY_SET|EMPTY_MAP|NONE|LEFT|CONS|LOOP|RIGHT|INT|SIZE|AND|XOR|ABS|NOT|LSL|LSR|STEPS_TO_QUOTA|CREATE_ACCOUNT|CREATE_CONTRACT|DEFAULT_ACCOUNT)\\b'},
         { token: 'constant.language.michelson',
           regex: '\\b(Unit|True|False|None|Pair|List|Map|Item|Set)\\b'},
         { token: 'keyword.operator.michelson.car',
           regex: '\\b(C(A|D)+R)\\b'},
         { token: 'keyword.operator.michelson.dup',
           regex: '\\b(DU+P)\\b'},
         { token: 'keyword.operator.michelson.dip',
           regex: '\\b(DI+P)\\b'},
         { token: 'string.michelson',
           regex: /"([^\\"]|\\.)*"/ },
         { token: 'comment.michelson',
           regex : "#.*$" },
         { token : "constant.numeric",
           regex : "[0-9]+\\b" },
         { token : "constant.numeric.hex",
           regex : "0[x][0-9a-fA-F]+\\b" },
         { token : "constant.numeric.octal",
           regex : "0[o][0-7]+\\b" },
         { token : "constant.numeric.binary",
           regex : "0[b][0-1]+\\b" },
         { token : "lparen",
          regex : "[({]"},
         { token : "rparen",
           regex : "[)}]"},
       ]
   };

}

oop.inherits(MichelsonHighlightRules, TextHighlightRules);

exports.MichelsonHighlightRules = MichelsonHighlightRules;
});
