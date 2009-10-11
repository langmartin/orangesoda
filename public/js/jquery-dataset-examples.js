(function($) {

     $(function() {
	 $('.example').each(setup_example);
	 example_1();
	 example_2();
	 example_3();
	 example_4();
	 example_5();
     });

     // All examples have the structure:
     //  <div class="example">
     //    <div class="markup">...</div>
     //    <div class="source">...</div>
     //    <div class="buttons">...</div>
     //  </div>

     function setup_example() {
	 var ex = $(this),
	     markup = ex.find('.markup'),
	     vismark = $('<div class="visible-markup" />');

	 vismark.insertAfter(markup);
	 update_visible_markup(ex);
	 $('<div class="result" />').hide().insertAfter($('.source', this));
     }

     function example(elem) {
	 return $(elem).parents('.example');
     }

     function result(example) {
	 return example.find('.result');
     }

     function update_visible_markup(example) {
	 example.find('.visible-markup').text(example.find('.markup').html());
	 return example;
     }

     // Example 1

     function example_1() {
	 var ex = example('#example-1').submit(get);

	 function get(ev) {
	     ev.preventDefault();
	     result(ex).html('The value of "a" is: ' + value()).show();
	 }

	 function value() {
	     return $('#example-1').dataset('a');
	 }
     }

     function example_2() {
	 var ex = example('#example-2').submit(set);

	 function set(ev) {
	     ev.preventDefault();
	     $('#example-2').dataset('a', $('#set-example-2-value').val());
	     update_visible_markup(ex);
	     result(ex).html('Value set!').show();
	 }

     }

     function example_3() {
	 var ex = example('#example-3').submit(get);

	 function get(ev) {
	     ev.preventDefault();
	     result(ex).html('The value is: ' + value()).show();
	 }

	 function value() {
	     return $.toJSON($('#example-3').dataset());
	 }
     }

     function example_4() {
	 var ex = example('#example-4').submit(set);

	 function set(ev) {
	     ev.preventDefault();

	     $('#example-4').dataset(json($('#set-example-4-value').val()));
	     update_visible_markup(ex);
	     result(ex).html('Value set!').show();
	 }

     }

     function example_5() {
	 var ex = example('#example-5').submit(remove);

	 function remove(ev) {
	     ev.preventDefault();

	     var value = $('#remove-example-5-value').val().split(/\s*,\s*/);
	     $('#example-5').removeDataset(value);
	     update_visible_markup(ex);
	     result(ex).html('Removed!').show();
	 }

     }

     function json(value) {
	 return eval("var __o = " + value + "; __o;");
     }

})(jQuery);
