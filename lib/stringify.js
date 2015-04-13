function stringify(tree) {
  if (!Array.isArray(tree)) {
    return String(tree);
  }
  return "(" + tree.map(stringify).join(" ") + ")";
}

module.exports = stringify;
