// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
// run_return.js file not correctly loaded when running radiant design ui.R
$(document).keydown(function(event) {
  // ...uploads don't have a visibility property/method ...
  if ($("#doe_download").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 79) {
    // file dialog pops up twice for some weird reason when using ui.R
    // ... but it works when running radiant from package ...
    // CMD-o to load factors
    $("#doe_upload").click();
    // document.getElementById("doe_upload").click();
    event.preventDefault();
  } else if ($("#doe_download").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 83) {
    // CMD-s to save factors
    document.getElementById("doe_download").click();
    event.preventDefault();
  }
});