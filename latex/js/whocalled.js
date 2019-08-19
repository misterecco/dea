window.onload = function () {
    setTimeout(function () {
        var ad = document.querySelector("ins.adsbygoogle");
        if (ad && ad.innerHTML.replace(/\s/g, "").length == 0) {
            $('.no-ads-info').show();
        }
        console.log('jest ok');
    }, 5000);
};