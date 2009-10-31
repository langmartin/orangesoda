jsmacs.elisp.data = function () {
  this.data = {};
  this.collecting = null;
  this.args = null;
};

jsmacs.elisp.data.prototype = {
  begin: function (type) {
    return {parent: this, collecting: type};
  },
  end: function () {
    return {parent: this, data: this.collecting(this.args)};
  }
};

jsmacs.parser = function (readch) {
  var state = "zero";
  var result = new jsmacs.elisp.data();

  var states = {
    zero: function (ch) {
      var state;
      switch (ch) {
      case "(": state = "lbegin"; break;
      case ")": state = "lend"; break;
      case "'": state = "quote"; break;
      case "`": state = "quasi"; break;
      case "[": state = "vbegin"; break;
      case "]": state = "vend"; break;
      case "?": state = "char"; break;
      default: state = "zero"; break;
      }
      if (whitespacep(ch)) state = "space";
      if (numericp(ch)) state = "number";
      return state;
    }
  };

  function lp () {
    var ch = readch();
    
  }
};
