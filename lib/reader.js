var sexp = require('sexp');
var fs = require('fs');

module.exports = function read(path) {
  return sexp(fs.readFileSync(path, 'utf8'));
};
