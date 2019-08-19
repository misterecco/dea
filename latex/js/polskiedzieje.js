$(window).load(function () {
    setTimeout(function () {
        //launch FIRST test (jQuery) - check adsense element height
        if ($('#adsense.an-sponsored').length > 0) {
            if ($('#adsense.an-sponsored .an-advert-banner').outerHeight() === 0) {
                $an_state = true;
                $('#adsense.an-sponsored').remove();
            }
        }

        //launch SECOND test (jQuery) - based on defined adverts selectors
        if ($an_state === null && anOptions.anOptionAdsSelectors !== '') {
            var substr = anOptions.anOptionAdsSelectors.split(',');
            $.each(substr, function (i) {
                if (($(substr[i]).length > 0 && $(substr[i]).outerHeight() === 0 )) {
                    $an_state = true;
                    return false;
                }
            });
        }

        //launch SECOND test with fuckadblock script (js file)
        //Disabled due to too many bug repports
        /*function adBlockDetected() {
            $an_state = true;
            //do action
            an_message_display($an_state);
            }
            function adBlockNotDetected() {
            //do action
            an_message_display($an_state);
            }
            if(typeof fuckAdBlock === 'undefined') {
            adBlockDetected();
            } else {
            fuckAdBlock.onDetected(adBlockDetected);
            fuckAdBlock.onNotDetected(adBlockNotDetected);
            }*/

        an_message_display($an_state);
    }, 500);
});