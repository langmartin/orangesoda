function letrec (obj) {
  for (key in obj) {
    var val = obj[key];
    if (isFunction(val)) {
      val = function () {
        this.__return.push({fn: val, args: arguments});
      };
    }
  }
}

function value (val) {
  while (this.__return.length > 0) {
    var fn = this.__return.pop();
    fn.fn.apply(this, fn.args);
  }
}


