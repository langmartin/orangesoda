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
   assert(
     function () {
       var result;
       foo.val(1);
       result = (foo.val() == 1);
       foo.let(2, function () {
                 result = result && (foo.val() == 2);
               });
       result = result && (foo.val() == 1);
       return result;
     }
   );

   function assert (thunk0, thunk1) {
     for (var ii=0; ii<arguments.length; ii++) {
       if (! arguments[ii]()) throw arguments[ii];
     }
   }
 }());
