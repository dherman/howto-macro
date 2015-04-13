var Env = require('immutable').Map;
var Macro = require("./lib/macro.js");
var read = require("./lib/reader.js");
var matches = require("./lib/matches.js");
var stringify = require("./lib/stringify.js");

// the initial expansion environment
var prelude = new Env({
  // builtin `quote` macro just short-circuits expansion
  "quote": new Macro(function(tree) { return tree; }),

  // builtin `lambda` macro binds its variables and expands the body
  "lambda": new Macro(function(tree, env) {
    // (lambda (params ...) body)
    var params = tree[1];
    var body = tree[2];

    var newEnv = params.reduce(function(prevEnv, param) {
      return prevEnv.set(param, "variable");
    }, env);

    var newBody = expandForm(body, newEnv);

    return ['%lambda', params, newBody];
  }),

  // builtin `begin` macro expands its subexpressions
  "begin": new Macro(function(tree, env) {
    // (begin body ...)
    var body = tree.slice(1);

    var newBody = body.map(function(subtree) {
      return expandForm(subtree, env);
    });

    return ['%begin'].concat(newBody);
  }),

  // builtin `let-syntax` macro parses a macro, binds it, and expands body
  "let-syntax": new Macro(function(tree, env) {
    // (let-syntax ((m macro) ...) body)
    var bindings = tree[1];
    var body = tree[2];

    var newEnv = bindings.reduce(function(prevEnv, pair) {
      // (m macro)
      return prevEnv.set(pair[0], Macro.parse(pair[1]));
    }, env);

    return expandForm(body, newEnv);
  }),

  // builtin `let` macro, just for convenience
  "let": new Macro(function(tree, env) {
    // (let ((x expr) ...) body)
    var params = tree[1].map(function(pair) { return pair[0]; });
    var exprs = tree[1].map(function(pair) { return pair[1]; });
    var body = tree[2];

    // ((lambda (x ...) body) expr ...)
    var expanded = [['lambda', params, body]].concat(exprs);

    return expandForm(expanded, env);
  })
});

function expandForm(tree, env) {
  // console.log("expanding:");
  // console.log("  " + stringify(tree));
  // console.log("");

  // atom
  if (matches.atom(tree)) {
    return tree;
  }

  // (macro args ...)
  else if (matches.macroCall(tree, env)) {
    var macro = env.get(tree[0]);
    var nextTree = macro.call(tree, env);
    // console.log("result of macro invocation:");
    // console.log("  " + stringify(nextTree));
    // console.log("");
    return expandForm(nextTree, env);
  }

  // (tree ...)
  else {
    return tree.map(function(subtree) {
      return expandForm(subtree, env);
    });
  }
}

function expand(tree) {
  return expandForm(tree, prelude);
}

var inputTree = read(process.argv[2]);
var outputTree = expand(inputTree);
console.log(stringify(outputTree));
