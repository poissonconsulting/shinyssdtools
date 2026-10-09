// Client-side translation handler for shinyssdtools
$(document).ready(function() {
  
  // Custom message handler for translation updates
  Shiny.addCustomMessageHandler('updateTranslations', function(data) {
    const { translations, language } = data;
    
    // Update all elements with data-translate attributes
    $('[data-translate]').each(function() {
      const key = $(this).attr('data-translate');
      const $element = $(this);
      
      // Skip elements that are inside Shiny outputs to avoid breaking bindings
      if ($element.closest('.shiny-plot-output, .shiny-html-output, .shiny-text-output, .shiny-image-output, .datatables').length > 0) {
        return; // Skip this element
      }
      
      const iconHtml = $element.find('.bi').length > 0 ? $element.find('.bi')[0].outerHTML : '';
      
      if (translations[key]) {
        // Step names sit beside a numbered marker, so they drop their own
        // number ("1. Data" -> "Data").
        if ($element.is('[data-strip-number]')) {
          $element.text(translations[key].replace(/^\s*\d+\.\s*/, ''));
          return;
        }
        if (iconHtml) {
          // Preserve icon and add translated text with spacing
          $element.html(iconHtml + '<span style="margin-left: 0.5rem;">' + translations[key] + '</span>');
        } else {
          // Just update text content
          $element.html(translations[key]);
        }
      }
    });
    
    // Update language-specific attributes; <html lang> tells screen readers
    // which language to read the page in.
    $('body').attr('data-language', language.toLowerCase());
    const codes = { english: 'en', french: 'fr', spanish: 'es' };
    document.documentElement.lang = codes[language] || 'en';
    
    console.log('Translations updated for language:', language);
  });
  
  // Initialize with default language indicator
  $('body').attr('data-language', 'english');
});