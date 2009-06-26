//// Sample output functions.

function ASPoutM (obj0, obj1) {
  ht.each(arguments, function (k, v) {
            Response.Write(v);
          });
  return aspoutM;
};

function ArrayoutM (obj0, obj1) {
  var lp, acc = Array.prototype.slice.call(arguments);
  return lp = function (obj0, obj1) {
    ht.each(arguments, acc.push);
    return lp;
  };
};

function ArrayoutM (obj0, obj1) {
  var lp, acc = Array.prototype.slice.call(arguments);
  return lp = function (obj0, obj1) {
    ht.each(arguments, acc.push);
    return lp;
  };
};

function stringoutM (obj0, obj1) {
  var lp, acc = arguments.join("");
  return lp = function (obj0, obj1) {
    acc += arguments.join("");
    return lp;
  };
};


//// HT parser

var ht;
ht = function (specs) {
  var self = ht.descend(specs);
  return function (outM, arg0, arg1) {
    var lp, args = Array.prototype.slice.call(arguments);
    ht.each(
      self,
      lp = function (key, val) {
        if (ht.isparsed(val))
          ht.displaywith(outM, val);
        else {
          if (ht.isobject)
            lp(val);
          else outM(val);
        }
      }
    );
  };
};

ht.descend = function (spec, child0, child1) {
  if (isarray(spec)) return ht.apply(this, spec);
  var parsed,
  self = ht.ugly_the_state_machine(spec);
  self.children = ht.map1(
    arguments,
    function (val) {
      if (isobject(val))
        return ht.apply(this, val);
      parsed = ht.ugly_the_state_machine(val);
      if (parsed.length == 1)
        return parsed.tag;
      return parsed;
    }
  );
  return self;
};

ht.self_closing_tags = {
  br: true,
  input: true,
  img: true
};

ht.displaywith = function (outM, parsed) {
  var tag = parsed.tag,
  skip = {tag:true, children:true};
  outM = outM("<", tag);
  ht.each(parsed, function (key, val) {
            if (skip[key]) return;
            outM = outM(" ", key, '="', val, '"');
          });
  outM = outM(">");
  ht.each(parsed.children, function (child) {
            outM = ht.displaywith(outM, child);
          });
  if (! ht.self_closing_tags[tag])
    outM = outM("</", tag, ">");
  return outM;
};

ht.isobject = function (obj) {
  return obj && (typeof obj == "object");
};

ht.isarray = function (obj) {
  return obj && (typeof obj == "object") && obj.length;
};

/// these are handy for mapping keys or values

ht.first = function (one) {
  return one;
};

ht.second = function (one, two) {
  return two;
};

ht.assert = function (section, thunks) {
  var thunk;
  for (var ii=1; ii<arguments.length; ii++) {
    thunk = arguments[ii];
    if (! (thunk())) {
      Response.Write(section);
      throw thunk;
    }
  }
};

ht.equal = function (obj0, obj1) {
  var result = true;
  ht.each(obj0, function (key, val) {
            if (! (val == obj1[key]))
              return result = false;
            return true;
          });
  return result;
};

ht.push = function(arr, val0, val1) {
  ht.each(arguments, function (ii, val) {
            arr.push(val);
          },
          {start: 1});
};

ht.extend = function (obj0, obj1) {
  var ii, obj, key;
  for (ii=1; ii<arguments.length; ii++) {
    obj = arguments[ii];
    for (key in obj) {
      obj0[key] = obj[key];
    }
  }
  return obj;
};

ht.each = function (obj, fn, opts0) {
  if (! obj) return false;
  var opts = {};
  var ii, val;
  var string = (typeof obj == "string") || false;
  ht.extend(opts, {start: 0, stop: obj.length, step: 1}, opts0);
  if (obj.length) {
    for (ii=opts.start; ii<opts.stop; ii+=opts.step) {
      val = (string) ? obj.charAt(ii) : obj[ii];
      if (fn(ii, val) === false) break;
    }
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
  },
  function () {
    var sum = 0;
    ht.each([1, 2, 3], function (i,v) {
              sum += v;
            });
    return sum == 6;
  },
  function () {
    var sum = 0;
    ht.each("123", function (i,v) {
              sum += parseInt(v);
            });
    return sum == 6;
  }
);

ht.each1 = function (lst, proc) {
  ht.each(lst, proc, {start:1});
};

ht.map = function (lst, proc, opts) {
  var acc = [];
  ht.each(lst,
          function (k, v) {
            acc.push(proc(k, v));
          },
          opts);
  return acc;
};

ht.assert(
  "map",
  function () {
    return ht.equal(
      [1,2,3],
      ht.map([3,4,5], function (k, v) {
               return v - 2;
             })
    );
  }
);

ht.map1 = function (lst, proc) {
  return ht.map(lst, proc, {start:1});
};

ht.escape = function (str) {
  var acc = [];
  ht.each(str, function (ii, ch) {
            switch (ch) {
            case "&": acc.push("&amp;"); break;
            case "<": acc.push("&lt;"); break;
            case ">": acc.push("&gt;"); break;
            case "\"": acc.push("&quot;"); break;
            default: acc.push(ch); break;
            }
          });
  return acc.join("");
};

ht.assert(
  "escape",
  function () {
    return "foo &lt;b&gt;" == ht.escape("foo <b>");
  }
);

ht.jsonScalar = function (val) {
  if (typeof val == "number") {
    return val;
  }
  if (! (typeof val == "string")) val += "";
  return '"'
    + val.replace(/[\t\r\n]/g, "").replace(/\"/g, "\\\"")
    + '"';
};

ht.json = function (obj) {
  var first = true,
  disp = ht.disp,
  list = ht.isarray(obj);
  disp((list) ? "[" : "{");
  ht.each(
    obj,
    function (key, val) {
      if (first) first = false;
      else disp(",");
      if (! list) {
        disp(ht.jsonScalar(key), ":");
      }
      if (ht.isobject(val)) ht.json(val);
      else disp(ht.jsonScalar(val));
    }
  );
  disp((list) ? "]" : "}");
};

ht.printf_symbols = {
  "%": function (args) {
    return "%";
  },
  "!": function (args) {
    return args.shift()();
  },
  "c": function (args) {
    var val = args.shift();
    if ((! val) || (val == "false")) val = "";
    else val = "checked";
    Response.Write("debug %c: " + val + "<br>");
    return val;
  },
  "d": function (args) {
    return parseInt(args.shift());
  },
  "f": function (args) {
    return parseFloat(args.shift());
  },
  "s": function (args) {
    return ht.escape(args.shift());
  },
  "u": function (args) {
    return escape(args.shift());
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
              if (ch == "%") state = true;
              else acc += ch;
            }
          });
  return acc;
};

ht.assert(
  "printing",
  function () {
    return ht.printf("foo %s bar", "<b>")
      == "foo &lt;b&gt; bar";
  },
  function () {
    return ht.printf("") == "";
  }
);

ht.debug = function (str) {
  return false;
};

//// This is a state machine parser.

ht.symbols = {
  "#": "id",
  ".": "class",
  "$": "name",
  ":": "type",
  "=": "value",
  "[": {attr: true}
};

ht.ugly_the_state_machine = function (str) {
  var attr = {},
  state = 0,
  key = "tag",
  acc = "";
  function append (key, acc) {
    if (acc) {
      if (! attr[key]) attr[key] = [];
      attr[key].push(acc);
    }
  }
  function collect (ch, body) {
    if (ch == "\\") state = "esc";
    else {
      if (body()) acc += ch;
      else acc = "";
    }
  }
  ht.each(str, function (ii, ch) {
            switch (state) {
            case "esc":
              acc += ch;
              break;
            case "key":
              collect(ch, function () {
                        if (ch == "=") {
                          key = acc;
                          state = "val";
                          return false;
                        }
                        return true;
                      });
              break;
            case "val":
              collect(ch, function () {
                        if (ch == "]") {
                          append(key, acc);
                          state = 0;
                          return false;
                        }
                        return true;
                      });
              break;
            default:
              collect(ch, function () {
                        if (ht.symbols[ch]) {
                          append(key, acc);
                          key = ht.symbols[ch];
                          if (key.attr) {
                            state = "key";
                          }
                          return false;
                        }
                        return true;
                      });
              break;
            }
          }
         );
  append(key, acc);
  return attr;
};

ht.parse = function (input, child) {
  return ht.parse.parse(
    ht.ugly_the_state_machine(input),
    child
  );
};


ht.assert(
  "parse concatenation",
  function () {
    return '<a at="1 2">bar</a>'
      == ht.parse.parse({tag:"a",at:[1,2]}, "bar");
  }
);

ht.assert(
  "parsing",
  function () {
    var h = ht(["div"])(), d = "<div></div>";
    return h == d;
  },
  function () {
    return ht(["div.foo"])() == "<div class=\"foo\"></div>";
  },
  function () {
    return ht(["div#foo$foo.bar.baz[href=http://foo.com/bar/baz]"])()
      == '<div id="foo" name="foo" class="bar baz" href="http://foo.com/bar/baz"></div>';
  },
  function () {
    return ht(["div.%s"])("foo")
      == "<div class=\"foo\"></div>";
  }
);
