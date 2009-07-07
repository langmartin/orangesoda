var stringOutputPort, functionOutputPort, aspOutputPort,
standardOutput, disp;

(function () {
   function port () {
     this.isPort = true;
     this.disp = stdDisp;
   };

   stringOutputPort = function (str) {
     port.call(this);
     this.data = [str];
   };
   stringOutputPort.prototype = {
     close: function (ch) {
       if (! ch) ch = '';
       return this.port.join(ch);
     },
     write: function (obj) {
       this.data.push(obj);
     }
   };

   aspOutputPort = function () {
     port.call(this);
   };
   aspOutputPort.prototype = {
     close: function () {
       Response.End();
     },
     write: function (obj) {
       Response.Write(obj);
     }
   };

   functionOutputPort = function (write) {
     port.call(this);
     this.write = write;
   };
   functionOutputPort.prototype = {
     close: noop
   };

   var debug = (this.console && console.debug)
     || (repl && repl.print)
     || noop;
   standardOutput = new fluid();
   standardOutput.val(
     new functionOutputPort(
       function (obj) {
         if (this.console) console.debug(obj);
         else if (this.repl) repl.print(obj);
       }
     )
   );

   disp = function (port0, arg0, arg1) {
     var port, args;
     if (port && port.isPort) {
       port = port0;
       args = Array.prototype.splice.call(arguments);
       args.shift();
     }
     else {
       port = standardOutput.val();
       args = arguments;
     }
     port.disp(args);
   };

   function descend (lst, fn) {
     var ii, val;
     for (ii=0; ii<lst.length; ii++) {
       val = lst[ii];
       if (val && val.length) descend(val, fn);
       else if (typeof val == "function") descend(val(), fn);
       else fn(val);
     }
   }

   function stdDisp (arg0, arg1) {
     descend(arguments, this.write);
   }

   function noop () {
     return true;
   }
 })();
