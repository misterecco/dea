window.onload = function() {
    if( window.canRunAds === undefined ){
        // adblocker detected, show fallback
        $('div#contentDiv').html("<b>Wylacz adblock na tej stronie. Tu jest tylko jedna mala niewinwazyjna reklama.</b>");
        //showFallbackImage();
    }
}