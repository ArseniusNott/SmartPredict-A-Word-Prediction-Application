$(window).on("load", function() {
  $(".predicted_word").on("click", function(){
    let to_predict_text_box = $("#to_predict_text_box");
    let initial_value = to_predict_text_box.val().trim();
    to_predict_text_box.val(initial_value + " " + $(this).val());
    to_predict_text_box.change();
  });
});