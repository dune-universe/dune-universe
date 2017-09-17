var Canvas = require('canvas');
var Image = Canvas.Image;
var fs = require('fs');
var pixelmatch = require('pixelmatch');
var drawing_tests = require('./src/drawing_tests_in_javascript.bc.js').drawing_tests;

function writeTo(canvas, fileName) {
  var out = fs.createWriteStream(fileName);
  var stream = canvas.pngStream();
  stream.on('data', function(chunk) {
    out.write(chunk);
  });
}

process.exitCode = 0;

for (var i = 1; i < drawing_tests.length; i++) {
  // There is probably a better way to do this.
  // But 'var test = drawing_tests[i]; fs.readFile...' make all callbacks use the last test.
  (function(test) {
    fs.readFile('drawing_tests/in_command_line/' + test.name + '.png', function(err, squid) {
      var cairo_canvas = new Canvas(test.width, test.height);
      var cairo_context = cairo_canvas.getContext('2d');
      if (err) throw err;
      img = new Image;
      img.src = squid;
      cairo_context.drawImage(img, 0, 0);
      var cairo_data = cairo_context.getImageData(0, 0, test.width, test.height);
      writeTo(cairo_canvas, 'drawing_tests/in_node/' + test.name + '.cairo.png')

      var html5_canvas = new Canvas(test.width, test.height);
      try {
        test.draw(html5_canvas);
      } catch(err) {
        console.log(test.name, err);
        process.exitCode = 1;
        return;
      }
      var html5_data = html5_canvas.getContext("2d").getImageData(0, 0, test.width, test.height);
      writeTo(html5_canvas, 'drawing_tests/in_node/' + test.name + '.node.png')

      var diff_canvas = new Canvas(test.width, test.height);
      var diff_context = diff_canvas.getContext("2d");
      var diff_data = diff_context.createImageData(test.width, test.height);
      var differences = pixelmatch(cairo_data.data, html5_data.data, diff_data.data, test.width, test.height, {threshold: 0.09, includeAA: false});
      diff_context.putImageData(diff_data, 0, 0);
      writeTo(diff_canvas, 'drawing_tests/in_node/' + test.name + '.diff.png')

      if(differences == 0) {
        // console.log("Success:", test.name);
      } else if(test.known_failure) {
        // console.log("Known failure:", test.name);
      } else {
        console.log("FAILURE:", test.name);
        process.exitCode = 1;
      }
    });
  })(drawing_tests[i]);
};
