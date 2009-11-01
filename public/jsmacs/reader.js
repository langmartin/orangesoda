var jsmacs = {elisp: {}};

(function () {
   var reader = jsmacs.elisp.reader = function (text) {
     var ii = 0;
     function peek () {
       if (ii > text.length) return false;
       return text.charAt(ii);
     }
     function read () {
       var ch = peek();
       ii = ii + 1;
       return ch;
     }
     return reader.parser(read, peek);
   };

   function whitespacep (ch) {
     return (ch.search(/^[ \r\n\t\v]$/) >= 0);
   }
   function numericp (ch) {
     return (ch.search(/^\d$/) >= 0);
   }

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

   function eReader (text) {
     throw new TypeError("Reader parse error: " + text);
   }

   reader.parser = function (read, peek) {
     var state = "zero";

     //// Inclusive states should start accumulating with the character
     //// they're passed, and should peek, reading only when they
     //// accumulate. Exclusive states should just read.

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

     function token (ch, cls) {
       if (cls == "lbegin") return list();
       else if (cls == "symbol") return symbol(ch);
       else if (cls == "string") return string(ch);
       else if (cls == "number") return number(ch);
       else if (cls == "quote") return ["quote", list()];
       else if (cls == "quasi") return ["quasiquote", list()];
       else throw eReader(cls);
     }

     function vector (head) {
       var vec = [];
       var ch, cls;
       while (true) {
         ch = read();
         if (ch === false) break;
         cls = charclass(ch);
         if (cls == "vend") break;
         vec.push(token(ch, cls));
       }
       return vec;
     }

     function list () {
       var ch, cls;
       ch = read();
       if (ch === false) return null;
       cls = charclass(ch);
       if (cls == "lend") return null;
       if (cls == "space") return list();
       return [token(ch, cls), list()];
     }

     return list();
   };
 })();
