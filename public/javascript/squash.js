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

   function map (arr, proc) {
     var result = [];
     for (var ii = 0; ii < arr.length; ii++) {
       result[ii] = proc(arr[ii], ii);
       if (result[ii] === false) break;
     }
     return result;
   }

   function counter (value, eject) {
     if (eject) return value;
     else return function (eject) {
       return counter(value + 1, eject);
     };
   }

   function pushif (arr, value) {
     if (value) arr.push(value);
     return arr;
   }

   // Interface.
   var eFrom = new TypeError("Use join to merge queries");
   var eJoin = new TypeError("Only one join permitted");

  function statement (env, prev) {
     this.env = env || {};
     // if (prev) this.prev = prev;
   };
   squash.fn = statement.prototype = {
     _clone: function () {
       var self = new statement();
       for (var key in this.env) {
         self.env[key] = this.env[key];
       }
       return self;
     },
     from: function (table, extra) {
       var self = this._clone(); var env = self.env;
       if (env.from) throw eFrom;
       env.from = {table: table, extra: extra};
       env.id = counter(0);
       return self;
     },
     _table: function (required) {
       var id = this.env.id;
       if (required) {
         if (! id) return this.env.join.right._table(required);
       }
       if (! id) return false;
       return "t" + id("eject");
     },
     select: function (columns) {
       if (!isArray(columns)) columns = slice.call(arguments);
       var self = this._clone(); var env = self.env;
       if (env.select) env.select = env.select.concat(columns);
       else env.select = columns;
       return self;
     },
     _select_opts: function (opt) {
       var self = this._clone(); var env = self.env;
       var keys = env.select_opts || {};
       keys[opt] = true;
       env.select_opts = keys;
       return self;
     },
     count: function (opt) {
       return this._select_opts("count");
     },
     distinct: function (opt) {
       return this._select_opts("distinct");
     },
     _where: function (col, op, val, concat) {
       var self = this._clone(); var env = self.env; var old = this.env;
       self.env.where = function (driver, table, clip) {
         var result = [], value;
         var field = driver.field(table, col);
         if (field) {
           value = (typeof val == "function") ? val() : val;
           value = driver.type(table, col, env).tosql(value);
           result.push(concat(field, op, value, driver));
           if ((!clip) && old.where) {
             result = result.concat(old.where(driver, table));
           }
         }
         return result;
       };
       return self;
     },
     where: function (col, op, val) {
       return this._where(
         col, op, val,
         function (col, op, val) {
           return [col, op, val].join(" ");
         });
     },
     wherenotnull: function (col, op, val) {
       return this._where(
         col, op, val,
         function (col, op, val) {
           col = ["ISNULL(", col, ", '')"].join('');
           return [col, op, val].join(" ");
         });
     },
     wherecol: function (col, op, col2) {
       var self = this._clone(); var env = self.env;
       var isJoined = env.join;
       var prev = env.where;
       env.where = function (driver, table, clip) {
         var result = [
           [driver.field(table, col), op, driver.field(table, col2)].join(" ")
         ];
         if ((!clip) && prev) result = result.concat(prev(driver, table));
         return result;
       };
       return self;
     },
     wherein: function (col, op, values) {
       var self = this._clone(); var env = self.env; var old = this.env;
       env.where = function (driver, table, clip) {
         var result = [driver.wherein(table, col, op, values)];
         if ((!clip) && old.where) {
           result = result.concat(old.where(driver, table));
         }
         return result;
       };
       return self;
     },
     or: function (left, right, op) {
       var self = this._clone(); var env = self.env; var old = this.env;
       op = op || " OR ";
       env.where = function (driver, table, clip) {
         var result = [];
         function clause (where) {
           var arr = where.env.where(driver, table, "clip");
           if (arr.length == 1) return arr[0];
           return ["(", arr.join(" AND "), ")"].join('');
         }
         result.push(
           ["(", [clause(left), clause(right)].join(op), ")"].join('')
         );
         if ((!clip) && old.where) {
           result = result.concat(old.where(driver, table));
         }
         return result;
       };
       return self;
     },
     and: function (left, right) {
       return this.or(left, right, " AND ");
     },
     join: function (how, right, handler) {
       var self = new statement();
       right = right._clone();
       how = how || "INNER JOIN";
       self.env.join = {left: this, how: how, right: right,
                        handler: handler};
       var id = (this.env.join) ? this.env.join.right.env.id : this.env.id;
       right.env.id = id();
       return self;
     },
     orderby: function (col, mod) {
       mod = mod || "orderby";
       var self = this._clone(); var env = self.env;
       var tab = self._table();
       env.modifier = env.modifier || [];
       env.modifier.push(function (driver) {
                           return driver[mod](tab, col);
                         });
       return self;
     },
     groupby: function (col) {
       return this.orderby(col, "groupby");
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
     eachstatement: function (proc) {
       function lp (self) {
         proc(self);
         if (self.env.join) {
           lp(self.env.join.left);
           lp(self.env.join.right);
         }
       }
       lp(this);
     },
     eachfield: function (proc) {
       this.eachstatement(
         function (self) {
           var cols = self.env.select;
           if (!cols) return;
           for (var ii=0; ii < cols.length; ii++) {
             proc(ii, cols[ii], self._table());
           }
         });
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
     function from (self, driver) {
       var tmp = [];
       var env = self.env;
       if (env.join) {
         tmp.push(from(env.join.left, driver));
         tmp.push(env.join.how);
         tmp.push(from(env.join.right, driver));
         env.join.handler(
           function (arg0, arg1) {
             tmp.push("ON");
             tmp = tmp.concat(slice.call(arguments));
           },
           function (field) {
             return driver.field(env.join.left._table(true), field);
           },
           function (field) {
             return driver.field(env.join.right._table(true), field);
           }
         );
       } else {
         tmp.push(self.env.from.table);
         tmp.push("AS", self._table());
         if (self.env.from.extra) tmp.push(self.env.from.extra);
       }
       return tmp.join(" ");
     }
     result.push("FROM");
     result.push(from(self, driver));

     // WHERE
     function where (self, driver) {
       var tmp = [];
       if (self.env.where) {
         tmp = self.env.where(driver, self._table());
       }
       if (self.env.join) {
         pushif(tmp, where(self.env.join.left, driver));
         pushif(tmp, where(self.env.join.right, driver));
       }
       return tmp.join(' AND ');
     }
     result.push("WHERE");
     result.push(where(self, driver));

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
   squash.default_driver = function () {};
   squash.default_driver.prototype = {
     field: function (tab, field) {
       if (! tab) return field;
       return [tab, field].join(".");
     },
     type: function (tab, field, env) {
       var key = squash.type[field] || "string";
       return squash.type_defs[key];
     },
     wherein: function (tab, field, op, values) {
       field = this.field(tab, field);
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
    "", zup, function (where, item, ver) {
      return where(item("name"), "=", ver("name"));
    });

  assert(
    function () {
      return "" + foo ==
        "SELECT t1.date, t1.class FROM fnDocuments AS t1 WHERE ";
    },
    function () {
      return bar.toString() ==
        "SELECT t1.date, t1.class FROM fnDocuments AS t1 WHERE "
        + "(t1.date > '09/27/2009 15:08:09' "
        + "OR t1.class = 'valu''''''e')";
    },
    function () {
      return "" + baz ==
        "SELECT t1.name FROM item AS t1 WHERE t1.name = 'lang'";
    },
    function () {
      return "" + zup ==
        "SELECT t1.name, t1.current FROM version AS t1"
        + " WHERE t1.current = 1";
    },
    function () {
      return "" + quux ==
        "SELECT t1.name, t2.name, t2.current "
        + "FROM item AS t1 INNER JOIN version AS t2 ON t1.name = t2.name "
        + "WHERE t1.name = 'lang' "
        + "AND t2.current = 1";
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
        "SELECT addr, t1.name FROM el AS t1 with (NO LOCK)"
        + " INNER JOIN mv AS t2 with (NO LOCK) ON t1.name = t2.name"
        + " WHERE addr like 'foo%' AND t1.name = 'lang'";
    },
    function () {
      zup = baz.wherein("foo", "=", [1, 2, 3]);
      return "" + zup ==
        "SELECT t1.name FROM item AS t1 WHERE "
        + "t1.foo IN ('1', '2', '3')"
        + " AND t1.name = 'lang'";
    },
    function () {
      var zup = baz.wherein("foo", "not like", [1, 2, 3]);
      return "" + zup ==
        "SELECT t1.name FROM item AS t1 WHERE "
        + "t1.foo NOT LIKE IN ('1', '2', '3')"
        + " AND t1.name = 'lang'";
    },
    function () {
      foo = foo.where("test", "=", "test");
      foo = foo.and(foo.where("date", ">", new Date("9/27/2009 15:08:09")),
                    foo.where("class", "=", "valu'''e"));
      return "" + foo ==
        "SELECT t1.date, t1.class FROM fnDocuments AS t1 WHERE "
        + "(t1.date > '09/27/2009 15:08:09' "
        + "AND t1.class = 'valu''''''e') "
        + "AND t1.test = 'test'";
    },
    function () {
      foo = squash("test").select("test", "t2").count().distinct().wherecol("a", "=", "b");
      return "" + foo ==
        "SELECT COUNT(DISTINCT(t1.test, t1.t2)) FROM test AS t1 WHERE t1.a = t1.b";
    },
    function () {
      foo = squash("test").select("test").orderby("test").where("foo", "=", 1).distinct();
      return "" + foo ==
        "SELECT DISTINCT t1.test FROM test AS t1 WHERE t1.foo = '1' ORDER BY t1.test";
    },
    function () {
      foo = squash("test").select("test").where("foo", "=", 1);
      bar = squash("test").select("bar").where("baz", "=", 1);
      foo = foo.join("", bar, function (on, foo, bar) {
                 on(foo("id"), "=", bar("id2"));
               });
      return "" + foo ==
        "SELECT t1.test, t2.bar FROM test AS t1 "
        + "INNER JOIN test AS t2 ON t1.id = t2.id2 "
        + "WHERE t1.foo = '1' AND t2.baz = '1'";
    },
    function () {
      bar = squash("bar").select("bar").where("bar", "=", "foo");
      foo = foo.join("", bar, function (on, foo, bar) {
                       on(foo("id"), "=", bar("id"));
                     });
      return "" + foo ==
        "SELECT t1.test, t2.bar, t3.bar FROM test AS t1 "
        + "INNER JOIN test AS t2 ON t1.id = t2.id2 "
        + "INNER JOIN bar AS t3 ON t2.id = t3.id "
        + "WHERE t1.foo = '1' AND t2.baz = '1' AND t3.bar = 'foo'";
    }
  );
};
