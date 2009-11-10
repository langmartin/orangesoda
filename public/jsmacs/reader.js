var jsmacs = {elisp: {}};

(function () {
   var stringport = function (text) {
     this.text = text;
     this.line = 1;
     this.column = 1;
     this.index = 0;
   };
   stringport.prototype = {
     peek: function () {
       if (this.index > this.text.length) return false;
       return this.text.charAt(this.index);
     },
     read: function () {
       var ch = this.peek();
       this.index = this.index + 1;
       this.column = this.column + 1;
       if (ch == "\n") {
         this.line = this.line + 1;
         this.column = 1;
       }
       return ch;
     }
   };

   var reader = jsmacs.elisp.reader = function (text) {
     var port = new stringport(text);
     return reader.parser(port);
   };

   function eReader (text) {
     throw new TypeError("Reader parse error: " + text);
   }

   reader.parser = function (port) {
     var peek = function () { return port.peek(); };
     var read = function () { return port.read(); };

     /// Inclusive states should start accumulating with the character
     /// they're passed, and should peek, reading only when they
     /// accumulate. Exclusive states should just read.
     function inclusive (head, legal) {
       var sym = [head];
       var ch;
       while (true) {
         ch = peek();
         if (ch === false) break;
         else if (! legal[charclass(ch)]) break;
         sym.push(read());
       }
       return sym.join('');
     }

     // This is only used by inclusive, I don't feel like creating an
     // inclusive symbol definition. I was using it everywhere, now I
     // have code duplication instead.
     function charclass (ch) {
       var cls;
       switch (ch) {
       case "(": cls = "lbegin"; break;
       case ")": cls = "lend"; break;
       case "'": cls = "quote"; break;
       case "`": cls = "quasi"; break;
       case "[": cls = "vbegin"; break;
       case "]": cls = "vend"; break;
       case "?": cls = "char"; break;
       case "\"": cls = "string"; break;
       }
       if (cls) return cls;
       else if (whitespacep(ch)) cls = "space";
       else if (numericp(ch)) cls = "number";
       else cls = "symbol";
       return cls;
     }

     function symbol (head) {
       return inclusive(
         head,
         {symbol: true, number: true}
       );
     }

     // FIXME only reads integers
     function number (head) {
       return parseInt(
         inclusive(head, {number: true}),
         10
       );
     }

     function string (head) {
       var str = [];
       var state = 0;
       var ch;
       while (true) {
         ch = read();
         if (ch === false) break;
         if (state == "esc") { state = 0; str.push(ch); }
         else {
           if (ch == "\\") state = "esc";
           else if (ch == '"') break;
           else str.push(ch);
         }
       }
       return new String(str.join(''));
     }

     function token (ch) {
       switch (ch) {
       case "(": return list();
       case "'": return ["quote", list()];
       case "`": return ["quasiquote", list()];
       case "[": return vector();
       case "?": return character();
       case "\"": return string();
       }
       if (numericp(ch)) return number(ch);
       if (whitespacep(ch)) return token(read());
       else return symbol(ch);
       throw eReader();
     }

     function vector (head) {
       var vec = [];
       var ch;
       while (true) {
         ch = read();
         if (ch === false) break;
         if (ch == "]") break;
         vec.push(token(ch));
       }
       return vec;
     }

     function list () {
       var ch = read();
       if (ch === false) return null;
       if (ch == ")") return null;
       if (whitespacep(ch)) return list();
       return [token(ch), list()];
     }

     return list();
   };
 })();
