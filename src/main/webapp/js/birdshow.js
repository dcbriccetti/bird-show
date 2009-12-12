var BIRDSHOW = {
    urlShowing: '',
    showBig: function(url) {
        if (BIRDSHOW.urlShowing === url) {
            BIRDSHOW.hideBig();
        } else {
            $('#bigImage').attr('src', url);
            $('#bigImageDiv').show('slow');
            BIRDSHOW.urlShowing = url
        }
    },
    hideBig: function() {
        $('#bigImageDiv').hide('slow');
        BIRDSHOW.urlShowing = ''
    }
};
$(document).ready(function() {
    $('#bigImageDiv').hide();
})
