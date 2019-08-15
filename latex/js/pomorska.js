show: function() {
    var e = this
        , t = this.params
        , n = new a.default;
    if (window.pp_adblock_is_off || !window.acceptable_ads_disallowed || n.get("adblockCookie"))
        this.showCoverEvent("off");
    else {
        this.showCoverEvent("on"),
        n.set("adblockCookie", "set", 0, 2),
        this.cover = document.querySelector("." + t.class_name_hashed),
        this.content = document.querySelector("." + t.class_name_hashed + "__content"),
        this.cols = document.getElementsByClassName(t.class_name_hashed + "__helpCol");
        var o = document.querySelector("." + t.class_name_hashed + "__addToWhiteList");
        this.leaveButton = this.cover.querySelector("." + t.class_name_hashed + "__enter");
        var s = this.cover.querySelector("." + t.class_name_hashed + "__showHelp")
            , r = this.cover.querySelector("." + t.class_name_hashed + "__closeHelp")
            , i = this.cover.querySelector("." + t.class_name_hashed + "__help");
        this.helpImageOne = this.cover.querySelector("." + t.class_name_hashed + "__helpImageOne"),
        this.helpImageTwo = this.cover.querySelector("." + t.class_name_hashed + "__helpImageTwo"),
        this.helpImageThree = this.cover.querySelector("." + t.class_name_hashed + "__helpImageThree"),
        document.body.classList.add("adBlockCover"),
        this.cover.style.display = "flex",
        o.addEventListener("mouseup", function(t) {
            return e.reloadSite(t)
        }),
        this.leaveButton.addEventListener("click", function() {
            return e.hideCover()
        }),
        s.addEventListener("click", function() {
            return e.showInstruction(i)
        }),
        r.addEventListener("click", function() {
            return e.hideInstruction(i)
        })
    }
}, ...