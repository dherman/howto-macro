var Rule = require("./rule.js");
var stringify = require("./stringify.js");

function Macro(call) {
  this.call = call;
}

Macro.parse = function(tree) {
  // (syntax-rules (keyword ...) ((pattern template)) ...)
  var rules = tree.slice(2).map(function(subtree) {
    var patt = subtree[0];
    var tmpl = subtree[1];
    // console.log("pattern:");
    // console.log("  " + stringify(patt));
    // console.log("");
    // console.log("template:");
    // console.log("  " + stringify(tmpl));
    // console.log("");
    return new Rule(patt, tmpl);
  });

  return new Macro(function(tree, env) {
    // try matching each rule in order till one succeeds
    for (var i = 0, n = rules.length; i < n; i++) {
      var matches = rules[i].match(tree);
      if (matches) {
        return rules[i].instantiate(matches);
      }
    }

    // no rules matched
    throw new Error("no pattern matches: " + tree);
  });
}

module.exports = Macro;
