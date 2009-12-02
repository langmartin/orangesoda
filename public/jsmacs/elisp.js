(function () {
   function environment (env) {
     var frame = {};
     frame.__proto__ = env;
     return frame;
   }

   function atomp (obj) {
     return typeof obj != "object";
   }

   var syntax = {
     lambda: lambda,
     if: _if_,
     defmacro: defmacro,
     defun: defun
   };

   function lambda (expr) {
   }

   function _if_ (env, expr) {
     return (evaluate(env, expr.car()))
       ? evaluate(env, expr.cadr())
       : evaluate(env, expr.caddr());
   }
     
   function evaluate (env, expr) {
     if (atomp(expr)) return expr;
     if (car(expr) == "lambda") return analyse(expr);
     if (lambdap(expr)) return analyse(expr);
     if (formp(expr)) {
       return evaluate(env, expand(env, expr));
     }
     else throw new TypeError("Invalid expression.");
   }

   function main (proc) {
     var env = environment({});
     while (proc) {
       proc = apply(proc, arguments);
     }
   }

})();
