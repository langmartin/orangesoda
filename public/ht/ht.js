function ht (spec, child) {
  var body;
  if (typeof child == "string") {
    body = lp([spec, child]);
  } else {
    body = lp(spec);
  }
  return function () {
    if (arguments.length == 0) return body;
    var args = Array.prototype.slice.call(arguments);
    args.unshift(body);
    return ht.printf.apply(this, args);
  };
  function lp (args) {
    var spec = args.shift();
    var children = "";
    ht.each(args, function (i, child) {
              children += (typeof child == "string") ? child
                : (typeof child == "function") ? child()
                : lp(child);
            });
    return ht.parse(spec, children);
  }
}

ht.assert = function (section, thunks) {
  var thunk;
  for (var ii=1; ii<arguments.length; ii++) {
    thunk = arguments[ii];
    if (! (thunk())) throw thunk;
  }
};

ht.extend = function (obj) {
  for (var ii=1; ii<arguments.length; ii++) {
    for (var key in arguments[ii]) {
      obj[key] = arguments[ii][key];
    }
  }
};

ht.each = function (obj, fn, opts0) {
  if (! obj) return false;
  var opts = {};
  var ii, val;
  ht.extend(opts, {start: 0, stop: obj.length, step: 1}, opts0);
  if (obj.length) {
    for (ii=opts.start; ii<opts.stop; ii+=opts.step)
      if (fn(ii, obj[ii]) === false) break;
  }
  else {
    for (ii in obj)
      if ((fn(ii, obj[ii])) === false) break;
  }
  return true;
};

ht.assert(
  "each",
  function () {
    var lst = [];
    ht.each([1,2,3,4,5], function (ii,val) {
              if (val == 4) return false;
              lst.push(val);
              return true;
            });
    return lst.length == 3;
  }
);

ht.escape = function (str) {
  var acc = "";
  ht.each(str, function (ii, ch) {
            switch (ch) {
            case "&": acc += "&amp;"; break;
            case "<": acc += "&lt;"; break;
            case ">": acc += "&gt;"; break;
            case "\"": acc += "&quot;"; break;
            default: acc += ch;
            }
          });
  return acc;
};

ht.printf_symbols = {
  "%": function (args) {
    return "%";
  },
  "a": function (args) {
    return escape(args.shift());
  },
  "s": function (args) {
    return ht.escape(args.shift());
  },
  "d": function (args) {
    return parseInt(args.shift());
  },
  "f": function (args) {
    return parseFloat(args.shift());
  }
};

ht.printf = function () {
  var state = false;
  var args = Array.prototype.slice.call(arguments);
  var template = args.shift();
  var acc = "";
  ht.each(template, function (ii, ch) {
            if (state) {
              if (ht.printf_symbols[ch]) {
                acc += ht.printf_symbols[ch](args); // mutates args!
              }
              state = false;
            }
            else {
              if (ch == "%") {
                state = "%";
                return;
              }
              else acc += ch;
            }
          });
  return acc;
};

ht.assert(
  "printing",
  function () {
    return ht.printf("foo %s bar %a", "<b>", "an attr")
      == "foo &lt;b&gt; bar an%20attr";
  },
  function () {
    return ht.printf("") == "";
  }
);

ht.debug = function (str) {
  // console.debug(str);
  // Response.Write("<!-- HT(debug): " + str + "-->\n");
  return false;
};

//// This is a straight-forward state machine parser. The obvious
//// symmetries could be knocked off, but I'll save that for later.
//// I'm taking bets on when later happens.

ht.symbols = {
  "#": "id",
  ".": "class",
  "$": "name",
  ":": "type",
  "[": {attr: true}
};

ht.ugly_the_state_machine = function (str) {
  var attr = {},
  state = 0,
  key = "tag",
  acc = "";
  ht.each(str, function (ii, ch) {
            switch (state) {
            case "esc":
              acc += ch;
              break;
            case "key":
              if (ch == "\\") state = "esc";
              else {
                if (ch == "=") {
                  key = acc;
                  acc = "";
                  state = "val";
                }
                else acc += ch;
              }
              break;
            case "val":
              if (ch == "\\") state = "esc";
              else {
                if (ch == "]") {
                  if (! attr[key]) attr[key] = [];
                  attr[key].push(acc);
                  acc = "";
                  state = 0;
                }
                else acc += ch;
              }
              break;
            default:
              if (ch == "\\") state = "esc";
              else {
                if (ht.symbols[ch]) {
                  if (! attr[key]) attr[key] = [];
                  attr[key].push(acc);
                  acc = "";
                  key = ht.symbols[ch];
                  if (key.attr) {
                    state = "key";
                  }
                }
                else acc += ch;
              }
              break;
            }
          }
         );
  return attr;
};

ht.parse = function (input, child) {
  var acc = ht.ugly_the_state_machine(input);
  var str = "<" + acc.tag;
  ht.each(acc, function (key, val) {
            if (! (key == "tag")) {
              str += " " + key + "=\""
                + val.join(" ") + "\"";
            }
          });
  return str + ">" + child + "</" + acc.tag + ">\n";
};

ht.assert(
  "parsing",
  function () {
    return ht(["div#foo$foo.bar.baz[href=http://foo.com/bar/baz]"])()
      == '<div id="foo" name="foo" class="bar baz" href="http://foo.com/bar/baz"></div>\n';
  }
);
