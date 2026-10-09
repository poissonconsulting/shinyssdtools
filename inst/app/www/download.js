// A download button with data-busy-download shows a spinner, and is disabled,
// from its click until the server sends downloadDone with its ID: a browser
// does not tell the page when a download has been prepared.
$(document).ready(function() {
  $(document).on('click', 'a[data-busy-download]', function() {
    $(this).addClass('ssd-downloading').attr({ 'aria-busy': 'true', 'aria-disabled': 'true' });
  });

  Shiny.addCustomMessageHandler('downloadDone', function(id) {
    $(document.getElementById(id)).removeClass('ssd-downloading').removeAttr('aria-busy aria-disabled');
  });
});
