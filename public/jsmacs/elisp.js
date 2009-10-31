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
})();
