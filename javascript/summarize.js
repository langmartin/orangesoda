(function () {
   function summarize (cont) {
     var form = this;
     var link = $("<a href=\"#\">" + $(form).val() + "</a>");
     $(form).replaceWith(link);
     $(link).click(function () {
                     $(this).unsummarize(form, cont);
                     return false;
                   });
     return link;
   };

   jQuery.fn.summarize = function (cont) {
     $.each(this, function () {
              summarize.call(this, cont);
            });
     return this;
   };

   jQuery.fn.unsummarize = function (form, cont) {
     var link = this;
     $(link).replaceWith(form);
     if (cont) cont.call(form);
     return this;
   };
 })();

// $(function () {
//     $("form input, form select").summarize(
//       function () {
//         $(this).change(function () {
//                          $("#save").show();
//                        });});
//     var self = $("#save")
//       .hide()
//       .click(function () {
//                $("form input, form select").summarize();
//                $(self).hide();
//              });
//   });
