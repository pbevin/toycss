var page = require('webpage').create();
var system = require('system');
var args = system.args;

console.log(args);

page.viewportSize = { width: 1024, height: 768 };
page.open(args[1], function(status) {
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
