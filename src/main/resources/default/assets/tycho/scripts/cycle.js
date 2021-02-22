/**
 * Provides a tiny helper for links which have a "cycle" class. For such links, on click, their contents
 * if exchanged with the value given in data-cycle. This is e.g. used by t:smartDateTime or t:smartDate
 * to display user friendly timestamps.
 */
sirius.ready(function () {
   document.querySelectorAll('.cycle').forEach(function(node) {
       if (sirius.isFilled(node.dataset.cycle)) {
           node.setAttribute('title', node.dataset.cycle);
           node.addEventListener('click', function() {
               let originalText = node.textContent.trim();
               node.textContent = node.dataset.cycle;
               node.setAttribute('title', originalText);
               node.dataset.cycle = originalText;
           });
       }
   });
});
