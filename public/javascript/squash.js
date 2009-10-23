var squash;

(function () {
   var slice = Array.prototype.slice;
   var end = null;
   var eRequired = new RangeError("Required statement missing.");

   squash = function (table, fields) {
     var self = new statement();
     return self.from(table, fields);
   };

   // Interface.
  function statement (env, prev) {
     this.env = env || {};
     // if (prev) this.prev = prev;
   };
   statement.prototype = {
     from: makemethod(from),
     where: makemethod(where),
     wherenotnull: makemethod(wherenotnull),
     or: makemethod(or),
     join: makemethod(join),
     eachfield: function (proc) {
       var ii = 0;
       for (var key in this.env.from.fields) {
         proc(ii, key, this.env.from.fields[key]);
         ii++;
       }
     }
   };

   // Support Functions.
   function identity (x) {
     return x;
   }

   function decint (number) {
     return parseInt(number, 10);
   }

   function makemethod (fn) {
     return function () {
       var args = slice.call(arguments);
       args.unshift(this.env);
       return new statement(fn.apply(this, args), this);
     };
   }

   function environment (env) {
     var self = {};
     self.from = env.from;
     self.where = env.where;
     self.join = env.join;
     return self;
   }

   // Statements.
   function from (env, table, fields) {
     if (env.from) throw TypeError("Use join to merge multiple from sets");
     var self = environment(env);
     self.from = {table: table, fields: fields};
     return self;
   }

   function whereval (env, driver, col, val) {
     return driver.type(env.from.table, col, env).tosql(
       (typeof val == "function") ? val() : val
     );
   }

   function where (env, col, op, val, fieldwrap) {
     fieldwrap = fieldwrap || identity;
     var self = environment(env);
     self.where = function (driver) {
       var result = [];
       var field = fieldwrap(driver.field(env.from.table, col));
       if (field) {
         var expr = [field, op, whereval(env, driver, col, val)].join(" ");
         if (env.where) result = env.where(driver);
         result.push(expr);
       }
       return result;
     };
     return self;
   }

   function or (env, left, right) {
     var self = environment(env);
     self.where = function (driver) {
       var result = [];
       var expr = ["(",
                   [left.env.where(driver).join(" AND "),
                    right.env.where(driver).join(" AND ")
                   ].join(" OR "),
                   ")"
                  ].join('');
       if (env.where) result = env.where(driver);
       result.push(expr);
       return result;
     };
     return self;
   }

   function wherenotnull (env, col, op, val) {
     return where(
       env, col, op, val,
       function (field) {
           return ["ISNULL(", field, ", '')"].join('');
       });
   }

   function join (env, other, handler) {
     if (env.join) throw TypeError("Only one join permitted");
     var self = environment(env);
     self.join = {other: other, handler: handler};
     return self;
   }

   // Export.
   statement.prototype.toString = function (driver) {
     driver = driver || new squash.default_driver();
     var env = this.env;

     // SELECT
     var result = ["SELECT"];
     function select (env) {
       var result = [];
       for (var field in env.from.fields) {
         var col = driver.field(env.from.table, field);
         if (col) result.push(col);
       }
       return result;
     }
     var tmp = select(env);
     if (env.join) tmp = tmp.concat(select(env.join.other.env));
     result.push(tmp.join(", "));

     // JOIN || FROM
     if (env.join) {
       result.push(
         env.join.handler(
           resolve(env), resolve(env.join.other.env)
         ).join(" ")
       );
     } else {
       result.push("FROM", env.from.table);
     }

     // WHERE
     result.push("WHERE");
     function where (env) {
       if (! env.where) return [];
       return env.where(driver);
     }
     tmp = where(env);
     if (env.join) tmp = tmp.concat(where(env.join.other.env));
     result.push(tmp.join(" AND "));

     return result.join(" ");

     function resolve (env) {
       var result = {};
       for (var key in env.from.fields) {
         var field = driver.field(env.from.table, key);
         if (field) result[key] = field;
       }
       return result;
     }
   };

   //// Type Definitions
   squash.type_defs = {};
   var def = squash.type_def = function (notation, tosql, toval) {
     squash.type_defs[notation] = {
       tosql: tosql,
       toval: toval
     };
   };

   def("string",
       function (val) {
         val += "";
         return ["'", val.replace(/\'/g, "''"), "'"].join('');
       },
       function (val) {
         return "" + val;
       });

   def("integer",
       function (val, lib) {
         var num = decint(val);
         if (isNaN(num)) throw new TypeError("Can't coerce a number");
         return num;
       },
       decint);

   def("date",
       function (val, lib) {
         return str(sqldate(val));
       },
       function (val) {
         return new Date(val);
       }); 

   function sqldate (date) {
     return [[date.getDate(), date.getMonth(), date.getFullYear()].join("/"),
             [date.getHours(), date.getMinutes(), 
              date.getSeconds()].join(":")
            ].join(" ");
   }

   //// Default Driver
   squash.default_driver = function () {};
   squash.default_driver.prototype = {
     field: function (tab, field) {
       return [tab, field].join(".");
     },
     type: function (tab, field, env) {
       var key = env.from.fields[field] || "string";
       return squash.type_defs[key];
     }
   };
 })();

squash.tests = function () {
  foo = squash("fnDocuments", {date: "date", "class": ""});
  bar = foo.or(foo.where("date", ">", new Date()),
               foo.where("class", "=", "valu'''e"));
  baz = squash("items", {date: "date", name: ""})
    .where("name", "=", "lang");
  zup = baz.join(
    squash("versions", {name: "", current: "integer"})
      .where("current", "=", "1"),
    function (item, ver) {
      return ["JOIN ON", item.name, "=", ver.name];
    });
};
