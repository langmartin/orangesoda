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
    return wrap(spec, children);
  }

  function wrap(spec, child) {
    var tag = spec.match(/^[^#.\[]*/) || spec;
    var id = spec.match(/#[^#.\[]*/);
    var cls = spec.match(/\.[^#.\[]*/g);

    var acc = "<" + tag;
    if (id) acc += " id=\"" + id[0].substr(1) + "\"";

    var val = "";
    ht.each(cls, function (i, m) {
              var v = m.substr(1);
              val += (val) ? " " + v : v;
            });
    if (val) acc += " class=\"" + val + "\"";

    acc += parse_attr(spec);
    acc += ">" + child + "</" + tag + ">\n";
    return acc;
  }

  function parse_attr(spec) {
    var attr = spec.match(/\[[^#.\[]*\]/g);
    var acc = "", aa = {};
    ht.each(attr, function (i, m) {
              var key = ht.lrchop(m.match(/[^=]*./)[0]);
              var val = ht.lrchop(m.match(/=.*$/)[0]);
              aa[key] = (aa[key]) ? aa[key] + " " + val : val;
            });

    for (key in aa) {
      acc += " " + key + "=\"" + aa[key] + "\"";
    }
    return acc;
  }
}

ht.lrchop = function (str) {
  if (! typeof str == "string") return null;
  return str.substring(1, str.length - 1);
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
  ht.extend(opts, {start: 0, stop: obj.length}, opts0);
  var ii;

  if (typeof obj == "string")
    for (ii=opts.start; ii<opts.stop; ii++)
      fn(ii, obj.charAt(ii));

  else if (obj.length)
  for (ii=opts.start; ii<opts.stop; ii++)
    fn(ii, obj[ii]);

  else
    for (ii in obj)
      fn(ii, obj[ii]);
  return true;
};

ht.printf = function () {
  var state = false;
  var args = Array.prototype.slice.call(arguments);
  var template = args.shift();
  var acc = "";

  ht.each(template, function (ii, ch) {
            if (! state) {
              if (ch == "%") {
                state = "%";
                return;
              }
              else acc += ch;
            }

            if (state) {
              switch (ch) {
              case "%": acc += "%"; break;
              case "s": acc += str(); break;
              }
              state = false;
            }
          });

  return acc;

  function str () {
    if (! args.length)
      throw {fn: "ht.printf",
             message: "too few arguments to substitute in template"};
    return ht.escape(args.shift());
  }
};

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

ht.debug = function (str) {
  Response.Write("ht.debug: " + str + "<br>");
};
