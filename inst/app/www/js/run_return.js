// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
$(document).keydown(function(event) {
  if ($("#doe_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#doe_run").click();
  } 
});
