// Busy states the server cannot show itself.

$(document).ready(function() {
  // A download button with data-busy-download shows a spinner, and is
  // disabled, from its click until the server sends downloadDone with its ID:
  // a browser does not tell the page when a download has been prepared.
  $(document).on('click', 'a[data-busy-download]', function() {
    $(this).addClass('ssd-downloading').attr({ 'aria-busy': 'true', 'aria-disabled': 'true' });
  });

  Shiny.addCustomMessageHandler('downloadDone', function(id) {
    $(document.getElementById(id)).removeClass('ssd-downloading').removeAttr('aria-busy aria-disabled');
  });

  // A button with data-sync-busy starts a slow job that runs in the session
  // (without daemons, see task_runner()), so the server sends nothing until it
  // is done. Its scope (data-sync-scope) shows its busy content from the click
  // until the server has been busy and is idle again.
  $(document).on('click', '[data-sync-busy]', function() {
    const scope = $(this).closest('[data-sync-scope]').addClass('ssd-syncing');
    let busy = false;
    $(document).on('shiny:busy.ssdsync', function() {
      busy = true;
    });
    $(document).on('shiny:idle.ssdsync', function() {
      if (busy) {
        scope.removeClass('ssd-syncing');
        $(document).off('.ssdsync');
      }
    });
  });
});
