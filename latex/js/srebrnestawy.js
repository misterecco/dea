$(function(){
    ...
    if (advertPolicy === false && typeof window.advertChecker === "undefined") {
        browserAlert.append(
            '<div class="alert alert-warning alert-dismissable">' +
                '<div class="container">' +
                    '<a href="#" class="close" data-dismiss="alert" aria-label="close" title="close">x</a>' +
                    'Do prawidlowego dzialania i wyswietlania sie strony nalezy wylaczyc wtyczke adblocka z Twojej przegladarki!' +
                '</div>' +
            '</div>');
        ...
    }
    ...
}