var page = require('webpage').create();
page.open('d.html', function(status) {
  var dimensions = page.evaluate(function() {
    return walk(document.body);

    function walk(n) {
      var i, log;

      var bb = n.getBoundingClientRect();
      log = [n.id || n.localName, bb.top, bb.right, bb.bottom, bb.left].join(" ") + "\n"

      for (i = 0; i < n.children.length; i++) {
        log += walk(n.children[i]);
      }

      return log;
    }
  });
  console.log(dimensions);
  phantom.exit();
});
