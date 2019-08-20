function adBlockDetected() {
    $('#07th202231').show();
}
function adBlockNotDetected() {
    $('#07th202231').hide();
}
if (typeof blockAdBlock === 'undefined') {
    adBlockDetected();
} else {
    blockAdBlock.onDetected(adBlockDetected).onNotDetected(adBlockNotDetected);
}