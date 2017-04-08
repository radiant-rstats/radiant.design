// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
$(document).keydown(function(event) {
  if ($("#doe_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#doe_run").click();
  }
});

$(document).keydown(function(event) {
  if ($("#doe_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#doe_report").click();
  } else if ($("#sample_size_comp_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#sample_size_comp_report").click();
  } else if ($("#sample_size_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#sample_size_report").click();
  } else if ($("#sampling_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#sampling_report").click();
  }
});