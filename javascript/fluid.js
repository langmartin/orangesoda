function fluid () {
  this.stack = [];
}

fluid.prototype = {
  val: function (val) {
    if (val) this.stack[0] = val;
    return this.stack[0];
  },

  let: function (val, thunk) {
    this.stack.unshift(val);
    try {
      thunk();
    } finally {
      this.stack.shift();
    }
  }
};

(function () {
   var foo = new fluid();
   ht.assert(
     "fluid",
     function () {
       var result;
       foo.val(1);
       result = (foo.val() == 1);
       foo.let(2, function () {
                 // Response.Write(foo.val());
                 result = result && (foo.val() == 2);
               }
              );
       return result;
     }
   );
 }());
