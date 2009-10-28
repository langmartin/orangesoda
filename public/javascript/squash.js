var squash;

(function () {
   var slice = Array.prototype.slice;
   var end = null;
   var eRequired = new RangeError("Required statement missing.");

   squash = function (table, extra) {
     var self = new statement();
     return self.from(table, extra);
   };

   // Support Functions.
   function identity (x) {
     return x;
   }

   function decint (number) {
     return parseInt(number, 10);
   }

   function isArray (obj) {
     return ((typeof obj == "object") && (obj.length !== undefined));
   }

   // Interface.
  function statement (env, prev) {
     this.env = env || {};
     // if (prev) this.prev = prev;
   };
   statement.prototype = {
     _clone: function () {
       var self = new statement();
       for (var key in this.env) {
         self.env[key] = this.env[key];
       }
       return self;
     },
     from: function (table, extra) {
       var self = this._clone(); var env = self.env;
       if (env.from) throw TypeError("Use join to merge queries");
       env.from = {table: table, extra: extra};
       return self;
     },
     select: function (columns) {
       if (!isArray(columns)) columns = slice.call(arguments);
       var self = this._clone(); var env = self.env;
       var key = (env.join) ? "select_join" : "select";
       if (env[key]) env[key] = env[key].concat(columns);
       else env[key] = columns;
       return self;
     },
     _where: function (col, op, val, tail) {
       var self = this._clone(); var env = self.env;
       var old = this.env;
       tail = tail || function (col, op, val) {
         return slice.call(arguments).join(" ");
       };
       var isJoined = old.join; // evaluate this at construction
       self.env.where = function (driver) {
         var result = [], value;
         var table = (isJoined) ? false : env.from.table;
         var field = driver.field(table, col);
         if (field) {
           value = (typeof val == "function") ? val() : val;
           value = driver.type(table, col, env).tosql(value);
           result.push(tail(field, op, value));
           if (old.where) result = result.concat(old.where(driver));
         }
         return result;
       };
       return self;
     },
     where: function (col, op, val) {
       return this._where(col, op, val);
     },
     or: function (left0, right0) {
       var self = this._clone(); var env = self.env; var old = this.env;
       env.where = function (driver) {
         var result = [];
         var left = left0.env.where(driver).join(" AND ");
         var right = right0.env.where(driver).join(" AND ");
         result.push(
           ["(", [left, right].join(" OR "), ")"].join('')
         );
         if (old.where) result = result.concat(old.where(driver));
         return result;
       };
       return self;
     },
     wherenotnull: function (col, op, val) {
       return this._where(
         col, op, val,
         function (col, op, val) {
           col = ["ISNULL(", col, ", '')"].join('');
           return [col, op, val].join(" ");
         }
       );
     },
     join: function (how, other, handler) {
       var self = this._clone(); var env = self.env;
       how = how || "JOIN";
       if (env.join) throw TypeError("Only one join permitted");
       env.join = {how: how, other: other, handler: handler};
       return self;
     },
     eachfield: function (proc) {
       function lp (cols, tab) {
         for (var ii in cols) proc(ii, cols[ii], tab);
       }
       var cols;
       if ((cols = this.env.select)) lp(this.env.select, this.env.from.table);
       if ((cols = this.env.select_join)) lp(cols);
       if (this.env.join) {
         var other = this.env.join.other.env;
         if ((cols = other.select)) lp(cols, other.from.table);
         if ((cols = other.select_join)) lp(cols);
       }
     }
   };

   // Export.
   statement.prototype.toString = function (driver) {
     driver = driver || new squash.default_driver();
     var env = this.env;
     var self = this;
     var result = [];

     // SELECT
     (function () {
        var col;
        var tmp = [];
        self.eachfield(
          function (ii, field, tab) {
              col = driver.field(tab, field);
              if (col) tmp.push(col);
          });
        result.push("SELECT", tmp.join(", "));
      })();

     // JOIN || FROM
     (function () {
        var tmp = [];
        function table (env) {
          tmp.push(env.from.table);
          if (env.from.extra) tmp.push(env.from.extra);
        }
        result.push("FROM");
        if (env.join) {
          table(env);
          tmp.push(env.join.how);
          table(env.join.other.env);
          env.join.handler(
            function (arg0, arg1) {
              tmp.push("ON");
              tmp = tmp.concat(slice.call(arguments));
            },
            function (field) {
              return driver.field(env.from.table, field);
            },
            function (field) {
              return driver.field(env.join.other.env.from.table, field);
            }
          );
          result.push(tmp.join(" "));
        } else {
          result.push(env.from.table);
          if (env.from.extra) result.push(env.from.extra);
        }
      })();

     // WHERE
     (function () {
        result.push("WHERE");
        function where (env, driver) {
          if (! env.where) return [];
          return env.where(driver);
        }
        var tmp = where(env, driver);
        if (env.join) tmp = tmp.concat(where(env.join.other.env, driver));
        result.push(tmp.join(" AND "));
      })();

     return result.join(" ");
   };

   //// Type Definitions
   squash.type_defs = {};
   var def = squash.type_def = function (notation, tosql, toval) {
     squash.type_defs[notation] = {
       tosql: tosql,
       toval: toval
     };
   };

   function sqlstring (val) {
     val += "";
     return ["'", val.replace(/\'/g, "''"), "'"].join('');
   }

   def("string",
       sqlstring,
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
         return sqlstring(sqldate(val));
       },
       function (val) {
         return new Date(val);
       });

   function sqldate (date) {
     return sprintf(
       "%02d/%02d/%04d %02d:%02d:%02d",
       (date.getMonth() + 1), date.getDate(), date.getFullYear(),
       date.getHours(), date.getMinutes(), date.getSeconds()
     );
   }

   //// Default Driver
   squash.default_driver = function () {};
   squash.default_driver.prototype = {
     field: function (tab, field) {
       if (! tab) return field;
       return [tab, field].join(".");
     },
     type: function (tab, field, env) {
       var key = squash.type[field] || "string";
       return squash.type_defs[key];
     }
   };
 })();

squash.tests = function () {
  function assert (thunk0, thunk1) {
    var thunk;
    for (var ii=0; ii<arguments.length; ii++) {
      thunk = arguments[ii];
      if (! (thunk())) {
        throw new Error("" + thunk);
      }
    }
  }

  squash.type = {date: "date", current: "integer"};
  foo = squash("fnDocuments").select(["date", "class"]);
  foo0 = foo;
  foo1 = foo.where("date", ">", new Date("9/27/2009 15:08:09"));

  assert(
    function () {
      return ((foo === foo0) && (foo !== foo1)
              && (foo.env === foo0.env) && (foo.env !== foo1.env));
    });

  bar = foo.or(foo.where("date", ">", new Date("9/27/2009 15:08:09")),
               foo.where("class", "=", "valu'''e"));

  baz = squash("item").where("name", "=", "lang").select(["name"]);
  zup = squash("version").where("current", "=", "1")
    .select(["name", "current"]);

  quux = baz.join(
    "INNER JOIN", zup, function (where, item, ver) {
      return where(item("name"), "=", ver("name"));
    });

  assert(
    function () {
      return "" + foo ==
        "SELECT fnDocuments.date, fnDocuments.class FROM fnDocuments WHERE ";
    },
    function () {
      return bar.toString() ==
        "SELECT fnDocuments.date, fnDocuments.class FROM fnDocuments WHERE "
        + "(fnDocuments.date > '09/27/2009 15:08:09' "
        + "OR fnDocuments.class = 'valu''''''e')";
    },
    function () {
      return "" + baz ==
        "SELECT item.name FROM item WHERE item.name = 'lang'";
    },
    function () {
      return "" + zup ==
        "SELECT version.name, version.current FROM version"
        + " WHERE version.current = 1";
    },
    function () {
      return "" + quux ==
        "SELECT item.name, version.name, version.current "
        + "FROM item INNER JOIN version ON item.name = version.name "
        + "WHERE item.name = 'lang' "
        + "AND version.current = 1";
    },
    function () {
      var foo = squash("el", "with (NO LOCK)").select("name")
        .where("name", "=", "lang");
      var bar = squash("mv", "with (NO LOCK)");
      tonk = foo.join("", bar, function (on, foo, bar) {
                        return on(foo("name"), '=', bar("name"));
                      });
      tonk = tonk.select("addr").where("addr", "like", "foo%");
      return "" + tonk ==
        "SELECT el.name, addr FROM el with (NO LOCK)"
        + " JOIN mv with (NO LOCK) ON el.name = mv.name"
        + " WHERE addr like 'foo%' AND el.name = 'lang'";
    }
  );
};
