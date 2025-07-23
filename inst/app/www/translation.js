// Client-side translation handler for shinyssdtools
$(document).ready(function() {
  
  // Custom message handler for translation updates
  Shiny.addCustomMessageHandler('updateTranslations', function(data) {
    const { translations, language } = data;
    
    // Update all elements with data-translate attributes
    $('[data-translate]').each(function() {
      const key = $(this).attr('data-translate');
      const iconHtml = $(this).find('.bi').length > 0 ? $(this).find('.bi')[0].outerHTML : '';
      
      if (translations[key]) {
        if (iconHtml) {
          // Preserve icon and add translated text with spacing
          $(this).html(iconHtml + '<span style="margin-left: 0.5rem;">' + translations[key] + '</span>');
        } else {
          // Just update text content
          $(this).html(translations[key]);
        }
      }
    });
    
    // Update language-specific attributes
    $('body').attr('data-language', language.toLowerCase());
    
    console.log('Translations updated for language:', language);
  });
  
  // Initialize with default language indicator
  $('body').attr('data-language', 'english');
});