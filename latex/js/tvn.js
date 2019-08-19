var isAdBlockEnabled = function() {
    if (void 0 === window.isAdBlockEnabledCheckCounter && (window.isAdBlockEnabledCheckCounter = 0),
    !document.body)
        return window.isAdBlockEnabledCheckCounter < 15 && setTimeout(function() {
            window.isAdBlockEnabledCheckCounter++,
            isAdBlockEnabled()
        }, 10),
        window.adBlockEnabled = !1;
    var e = document.createElement("div");
    e.setAttribute("class", "sponsored-ad adoSlave advertisement pub_300x250 pub_300x250m pub_728x90 text-ad textAd text_ad text_ads text-ads text-ad-links"),
    e.setAttribute("style", "width: 1px !important; height: 1px !important; position: absolute !important; left: -1000px !important; top: -1000px !important;"),
    document.body.appendChild(e);
    var t = $(e)
        , i = $(e).is(":visible");
    return t.remove(),
    window.adBlockEnabled = !i,
    !i
};
...
v.isAdBlockEnabledAgain = function(e, t) {
    var i = nuvi.settings.accessProtocol + "//s1-player5.cdntvn.pl/advert.js";
    -1 !== i.indexOf("tvwisla.com.pl") && -1 === i.indexOf("cdntvn.stage.online") && (i = (i = i.replace("player5", "s1-player5")).replace(".stage.online", "-cdntvn.stage.online")),
    $.ajax({
        url: i,
        type: "GET",
        cache: !1,
        async: !1,
        success: function() {
            return v.adblockEnabledAgain = !1,
            "function" == typeof e && e(),
            !1
        },
        error: function() {
            return v.adblockEnabledAgain = !0,
            v.options().adBlockCheck && !tvn24ScrollTvnCheck() ? "function" == typeof t && t(v) : "function" == typeof e && e(),
            !0
        }
    })
}