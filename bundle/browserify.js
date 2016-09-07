/*
 * This file needs to be called with two parameters, the first parameter is an input file
 * and the second parameter is an output file. The input file contains a list of dependencies,
 * which are all loaded, afterwards bundled together and at the end written to the output file.
 */
var inf = ''
var outf = ''

if (process.argv.length > 3) {
  inf = process.argv[2]
  outf = process.argv[3]
}
else
  throw "Browserify can't be run because no input and output files were specified."

var fs = require("fs")
var out = fs.createWriteStream(outf)

var browserify = require('browserify');
var b = browserify(inf, { standalone: 'Bundle' });
b.bundle().pipe(out)
