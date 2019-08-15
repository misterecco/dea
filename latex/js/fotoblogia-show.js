Service.showAdBlockPopup = function() {
    window.__serviceAbModal && ($(window.__serviceAbModal).appendTo("body"),
    window.__serviceAbModal = null);
    var t = $("div.modal__overlayer.modal__body-adblock");
    if (t.length) {
        t.show();
        try {
            ga("send", "event", "adblock-popup-v2", "show", "", {
                nonInteraction: !0
            })
        } catch (a) {}
        $("a.btn.ok", t).click(function() {
            try {
                ga("send", "event", "adblock-popup-v2", "settings")
            } catch (t) {}
        }),
        $("a.btn.cancel", t).click(function() {
            try {
                ga("send", "event", "adblock-popup-v2", "refresh")
            } catch (t) {}
            $.cookie("abpcancel", "1", {
                domain: "." + serviceRootDomain
            }),
            location.reload()
        })
    }
}