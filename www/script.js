// script.js
// Small UX touch: whenever Shiny re-renders the page frame (i.e. the
// user moved to a new wizard step), scroll back to the top of the
// window so the new page is never seen "half scrolled".
$(document).on('shiny:value', function(event) {
  if (event.name === 'page_frame') {
    window.scrollTo({ top: 0, behavior: 'smooth' });
  }
});
