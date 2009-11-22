(function () {
   var car = jsmacs.list.prototype.car;

   function isatom (expr) {
     return true;
   }
   function doatom (expr) {
     return expr;
   }

   function iscall (expr) {
     
   }
   function docall (proc, args, env, fenv) {
     for (var key in fenv_bindings(proc)) {
       env[key] = eval(args.shift, env, fenv);
     }
     
   }

   jsmacs.eval = function (form, env, fenv) {
     var word = car(form);
     if (isatom(word)) 1;
     if (isform(word)) 1;
     if (isprim(word)) 1;
     if (iscall(word)) 1;
   };



   function environment (env) {
     var frame = {};
     frame.__proto__ = env;
     return frame;
   }

   function atomp (obj) {
     return typeof obj != "object";
   }

   function lambdap (obj) {
     return expressionp(obj) && (car(obj) == "lambda");
   }

   function analyse (expr) {
     
   }

   function evaluate (env, expr) {
     if (atomp(expr)) return expr;
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
