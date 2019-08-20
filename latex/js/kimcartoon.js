if (adbWarn == null) {
    $.ajaxSetup({
        cache: false
    });
    var alb = true;
    $.getScript('https://pubmatic.com/wp-content/themes/pubmatic/js/jquery.alignHeight.js?ver=1.0').done(function(script, textStatus) {
        alb = false;
    });
    $.getScript('https://propellerads.com/wp-content/plugins/radiantthemes-addons/tabs/js/radiantthemes-tab-element-four.js').done(function(script, textStatus) {
        alb = false;
    });
    $.getScript('https://www.bebi.com/js/plugins.js').done(function(script, textStatus) {
        alb = false;
    });
    setTimeout(function() {
        if (alb) {
            $('#adbWarnContainer').css({
                display: "block"
            });
        }
    }, 8000);
}