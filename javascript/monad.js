/* The monad plumbing came mostly from here:
 * http://github.com/VincentToups/emacs-utils/blob/c3995fb7011380353687779fc229906d0bbfa4aa/README.md
 * 
 * I ported it into javascript. The spec may contain a writeup on my motivation
 * for doing things this way, if you don't grok it, restrain your fiddling to
 * the parser definitions at the bottom.
 */

var identityM = {
  result: function (val) { return val; },
  bind: function (result, fn) { return fn(result); }
};

var maybeM = 
  (function () {
     function Just (val) { return ["Just", val]; }
     function None (val) { return ["None", val]; }
     function isNone (obj) {
       return (obj && obj[0] && (obj[0] == "None"));
     }

     return {
       result: function (val) { return Just(val); },
       bind: function (result, fn) {
         if (isNone(result)) return result;
         else return fn(result[1]);
       }
     };
   })();

var seqM = {
  result: function (val) { return [val]; },
  bind: function (result, fn) {
    var tmp = $.map(result, fn), first = tmp.shift();
    return first.append(tmp);
  }  
};

var stateM = {
  result: function (val) {
    return function (state) { return [val, state]; };
  },
  bind: function (result, fn) {
    return function (state) {
      var tmp = result(state), val = tmp[0], state1 = tmp[1];
      return fn(val)(state1);
    };
  }
};

/* Syntactic sugar, i.e., the exported interface.
 */

// function resultM (monad, val) {
//   return monad.result(val);
// }

// function bindM (monad, val, fn) {
//   return monad.bind(val, fn);
// }

function doM (monad) {
  if (arguments.length == 1) return monad;
  else if (! ((arguments.length % 2) && (arguments.length >= 2))) {
    throw "doM: an even number of binding forms are required";
  } else {
    var rest = Array.prototype.slice.call(arguments).slice(2);
    return arguments[1](monad, doM.apply(this, rest));
  }
}

function letM (monad, bindings, body) {
  if (bindings.length == 1) return monad;
  else if (! ((bindings.length % 2) && (bindings.length >= 2))) {
    throw "doM: an even number of binding forms are required";
  } else {
    var rest = bindings.slice(2);
    return bindings[1](monad, letM.apply(this, rest));
  }
}
