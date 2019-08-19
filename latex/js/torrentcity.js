function e() {
    var t = "adsSupported";
    var r = window.document.createElement("script");
    r.setAttribute("async", "async");
    r.setAttribute("src", c + "ads.js");
    r.addEventListener("error", function() {
        f(false);
    });
    r.addEventListener("load", function() {
        var e = 0;
        function r() {
            if (!window[t]) {
                if (e++ < 10)
                    window.setTimeout(r, 100);
                else
                    f(false);
            } else {
                setTimeout(function() {
                    if (!window[t]) {
                        f(false);
                    } else {
                        f(window[t].offsetHeight !== 0);
                        if (window[t].parentElement)
                            window[t].parentElement.removeChild(window[t]);
                        return;
                    }
                }, 50)
            }
        }
        r();
    });
    window.document.body.appendChild(r);
}