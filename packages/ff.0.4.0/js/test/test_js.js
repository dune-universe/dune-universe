var FF = require("./FiniteField.js");
let x = FF.random();
let y = FF.random();
console.log(FF.toString(x) + " + " + FF.toString(y) + " = " + FF.toString(FF.add(x, y)));
