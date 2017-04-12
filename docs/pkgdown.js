$(function() {
  $("#sidebar").stick_in_parent({
    offset_top: $("#sidebar").offset().top
  });
  $('body').scrollspy({
    target: '#sidebar'
  });
});
