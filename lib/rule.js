var Matches = require('immutable').Map;
var stringify = require("./stringify.js");

function Rule(pattern, template) {
  this.pattern = pattern;
  this.template = template;
}

Rule.prototype.match = function(tree) {
  return match(this.pattern, tree);
};

Rule.prototype.instantiate = function(matches) {
  return instantiate(this.template, matches);
};

function match(pattern, tree) {
  // console.log("matching:");
  // console.log("  " + stringify(pattern));
  // console.log("  " + stringify(tree));
  // console.log("");

  // pattern variables match any tree
  if (typeof pattern === 'string') {
    return new Matches().set(pattern, tree);
  }

  // numbers match iff the numbers are equal
  if (typeof pattern === 'number') {
    return pattern === tree ? new Matches() : null;
  }

  // array patterns match recursively
  if (!Array.isArray(tree) || tree.length !== pattern.length) {
    return null;
  }
  if (tree.length === 0) {
    return new Matches();
  }
  var result = new Matches();
  for (var i = 0, n = tree.length; i < n; i++) {
    var submatches = match(pattern[i], tree[i]);
    if (!submatches) {
      return null;
    }
    result = result.merge(submatches);
  }
  return result;
}

function instantiate(template, matches) {
  if (typeof template === 'string') {
    // replace pattern variables and leave non-pattern variables as-is
    return matches.get(template, template);
  }

  if (typeof template === 'number') {
    return template;
  }

  return template.map(function(subtemplate) {
    return instantiate(subtemplate, matches);
  });
}

module.exports = Rule;
