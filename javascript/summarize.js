(function () {
   var text, dispatch = {
     text: text = function (inp) {
       return $(inp).val();
     },
     "select-one": function (inp) {
       return $("option[selected]", inp).text();
     },
     textarea: text
   };

   jQuery.fn.summarize = function () {
     $.each(this, summarize);
   };

   function summarize () {
     var data = $(this).data("summarize"),
     form, link, fn;

     if (! data) {
       data = {};
       data.form = form = this;
       $(form).change(function () {
                        $(this).summarize();
                        return false;
                      });
       fn = data.fn = dispatch[$(this).attr("type")];
       if (! data.fn) return this;
       data.link = $("<a href=\"#\">" + fn(form) + "</a>")
         .click(function () {
                  $(this).summarize();
                  return false;
                });
       data.state = "form";
       $(form).data("summarize", data);
       return $(form).summarize();
     }

     form = data.form, link = data.link;

     if (data.state == "form") {
       $(link).html(data.fn(form));
       $(this).replaceWith(link);
       data.state = "link";
       return this;
     }

     else {
       $(this).replaceWith(form);
       data.state = "form";
       return this;
     }
   };
 })();
