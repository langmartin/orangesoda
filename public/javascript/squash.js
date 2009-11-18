var squash;

(function () {
   // Utilities
   var slice = Array.prototype.slice;
   var end = null;
   var eRequired = new RangeError("Required statement missing.");
   var eFrom = new TypeError("Use join to merge queries");
   var eJoin = new TypeError("Only one join permitted");

   function identity (x) {
     return x;
   }

   function decint (number) {
     return parseInt(number, 10);
   }

   function isArray (obj) {
     return ((typeof obj == "object") && (obj.length !== undefined));
   }

   function map (arr, proc) {
     var result = [];
     for (var ii = 0; ii < arr.length; ii++) {
       result[ii] = proc(arr[ii], ii);
       if (result[ii] === false) break;
     }
     return result;
   }

   // Interface.
   squash = function (table, extra) {
     var self = new statement();
     self.from(table, extra);
     return self;
   };

   // Internal.
   function source (table, extra, tag) {
     this.env = {
       table: table,
       tableopts: extra,
       from: tag
     };
   }

   source.prototype = {
     _select: function (key, columns) {
       var env = this.env;
       if (isArray(columns[0])) columns = columns[0];
       else columns = slice.call(columns);
       var from = env.from || "unspecified";
       env.select[from] = columns;
       return this;
     },
     select: function (columns) {
       return this._select("select", arguments);
     },
     count: function (columns) {
       return this._select("count", arguments);
     },
     distinct: function (columns) {
       return this._select("distinct", arguments);
     },
     _where: function (column, operator, value) {
       var env = this.env;
       var tail = this.env.where;
       env.where = function (driver, clip) {
         driver.from = env.from;
         var result = [];
         result.push(driver.where(column, operator, value));
         if ((!clip) && tail) {
           result.concat(tail.call(this));
         }
         return result;
       };
       return this;
     },
     where: function (col, op, val) {
       return this._where("where", col, op, val);
     },
     wherenotnull: function (col, op, val) {
       return this._where("wherenotnull", col, op, val);
     },
     condition: function (col, op, col2) {
       return this._where("condition", col, op, val);
     },
     wherein: function (col, op, values) {
       var self = this._clone(); var env = self.env; var old = this.env;
       env.where = function (driver, clip) {
         var result = [driver.wherein(env.from.table, col, op, values)];
         if ((!clip) && old.where) {
           result = result.concat(old.where(driver));
         }
         return result;
       };
       return self;
     },
     or: function (left, right, op) {
       var self = this._clone(); var env = self.env; var old = this.env;
       op = op || " OR ";
       env.where = function (driver, clip) {
         var result = [];
         function clause (where) {
           var arr = where.env.where(driver, "clip");
           if (arr.length == 1) return arr[0];
           return ["(", arr.join(" AND "), ")"].join('');
         }
         result.push(
           ["(", [clause(left), clause(right)].join(op), ")"].join('')
         );
         if ((!clip) && old.where) {
           result = result.concat(old.where(driver));
         }
         return result;
       };
       return self;
     },
     and: function (left, right) {
       return this.or(left, right, " AND ");
     },
     orderby: function (col, mod) {
       mod = mod || "orderby";
       var self = this._clone(); var env = self.env;
       var tab = (env.join) ? false : env.from.table;
       env.modifier = env.modifier || [];
       env.modifier.push(function (driver) {
                           return driver[mod](tab, col);
                         });
       return self;
     },
     groupby: function (col) {
       return this.orderby(col, "groupby");
     }
   };

   function statement () {
     this.counter = 1;
     this.table = {};
     this.current = "";
   };

   squash.fn = statement.prototype = {
     copy: function () {
       var self = new statement();
       for (var key in this.env) {
         self.env[key] = this.env[key];
       }
       return self;
     },
     from: function (table, tag, extra) {
       if (! tag) {
         tag = "t" + this.counter;
         this.counter = this.counter + 1;
       }
       var self = new table(table, extra, tag);
       this.table[tag] = self;
       return this;
     },
     join: function (how, other, handler) {
       var self = this._clone(); var env = self.env;
       how = how || "JOIN";
       if (env.join) throw eJoin;
       env.join = {how: how, other: other, handler: handler};
       return self;
     },
     driver: function (driver) {
       var self = this._clone(); var env = self.env;
       env.driver = driver;
       return self;
     },
     filter: function (filter) {
       var self = this._clone(); var env = self.env;
       var prev = env.filter || false;
       if (prev) {
         env.filter = function (rec) {
           return filter(prev(rec));
         };
       }
       else env.filter = filter;
       return self;
     },
     eachfield: function (proc) {
       function lp (cols, tab) {
         for (var ii=0; ii<cols.length; ii++) proc(ii, cols[ii], tab);
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
     driver = driver || this.env.driver || new squash.default_driver();
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
        var select = ["SELECT "]; var key;
        if (env.select_opts) {
          if (env.select_opts.count) {
            for (key in env.select_opts) select.push(key.toUpperCase() + "(");
          }
          else if (env.select_opts.distinct) {
            select.push("DISTINCT ");
          }
          select.push(tmp.join(", "));
          if (env.select_opts.count) {
            for (key in env.select_opts) select.push(")");
          }
        }
        else {
          select.push(tmp.join(", "));
        }
        result.push(select.join(''));
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

     // ORDER BY & GROUP BY
     (function () {
        if (env.modifier) {
          for (var ii=0; ii < env.modifier.length; ii++) {
            result.push(env.modifier[ii](driver));
          }
        }
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
   squash.util = {};
   squash.util.sqlstring = sqlstring;
   function sqlstring (val) {
     val += "";
     return ["'", val.replace(/\'/g, "''"), "'"].join('');
   };

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
   squash.default_driver = function () {
     this.squash = null;
     this.from = null;
   };
   squash.default_driver.prototype = {
     field: function (field) {
       var tab = this.squash.env.from;
       if (! tab) return field;
       return [tab, field].join(".");
     },
     type: function (tab, field, env) {
       var key = squash.type[field] || "string";
       return squash.type_defs[key];
     },
     where: function (col, op, val) {
       return [this.field(col), op, this.tosql(val)].join(' ');
     },
     wherenotnull: function (col, op, val) {
       return this.where(col, op, val);
     },
     wherein: function (field, op, values) {
       field = this.field(field);
       values = map(values, this.type("", field).tosql);
       op = op.toLowerCase();
       var operations = {
         "=": " IN ",
         "<>": " NOT IN ",
         "like": " LIKE IN ",
         "not like" : " NOT LIKE IN "
       };
       return [field, operations[op], "(", values.join(", "), ")"].join('');
     },
     orderby: function (tab, col) {
       return "ORDER BY " + this.field(tab, col);
     },
     groupby: function (tab, col) {
       return "GROUP BY " + this.field(tab, col);
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
    },
    function () {
      var zup = baz.wherein("foo", "=", [1, 2, 3]);
      return "" + zup ==
        "SELECT item.name FROM item WHERE "
        + "item.foo IN ('1', '2', '3')"
        + " AND item.name = 'lang'";
    },
    function () {
      var zup = baz.wherein("foo", "not like", [1, 2, 3]);
      return "" + zup ==
        "SELECT item.name FROM item WHERE "
        + "item.foo NOT LIKE IN ('1', '2', '3')"
        + " AND item.name = 'lang'";
    },
    function () {
      foo = foo.where("test", "=", "test");
      foo = foo.and(foo.where("date", ">", new Date("9/27/2009 15:08:09")),
                    foo.where("class", "=", "valu'''e"));
      return "" + foo ==
        "SELECT fnDocuments.date, fnDocuments.class FROM fnDocuments WHERE "
        + "(fnDocuments.date > '09/27/2009 15:08:09' "
        + "AND fnDocuments.class = 'valu''''''e') "
        + "AND fnDocuments.test = 'test'";
    },
    function () {
      foo = new squash("test").select("test", "t2").count().distinct().wherecol("a", "=", "b");
      return "" + foo ==
        "SELECT COUNT(DISTINCT(test.test, test.t2)) FROM test WHERE test.a = test.b";
    },
    function () {
      foo = new squash("test").select("test").orderby("test").where("foo", "=", 1).distinct();
      return "" + foo ==
        "SELECT DISTINCT test.test FROM test WHERE test.foo = '1' ORDER BY test.test";
    }
  );
};
