 var r = !!$.cookie("abpcancel");
!r && window.WP && "function" == typeof window.WP.crux.sealed && 1 == window.WP.crux.sealed() && setTimeout(function() {
    a.pause(),
    Service.showAdBlockPopup()
}, 100), ...