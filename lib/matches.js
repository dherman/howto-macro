var Macro = require("./macro.js");

function atom(tree) {
  return (Array.isArray(tree) && tree.length === 0) ||
         typeof tree === 'number' ||
         typeof tree === 'string';
}

function macroCall(tree, env) {
  if (!Array.isArray(tree)) {
    return false;
  }
  var operator = tree[0];
  return typeof operator === 'string' &&
         env.has(operator) &&
         env.get(operator) instanceof Macro;
}

exports.atom = atom;
exports.macroCall = macroCall;
