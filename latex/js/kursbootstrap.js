// In "advertisement.js"
document.write('<div id="tester">an advertisement</div>');

// In the main page, inline
if (document.getElementById("tester") != undefined) {
    document.write('<div class="alert alert-success alert-dismissible" role="alert"><button type="button" class="close" data-dismiss="alert"><span aria-hidden="true">&times;</span><span class="sr-only">Zamknij</span></button><strong>Dziekuje!</strong> Widze, ze nie uzywasz AdBlocka. To dobrze:) Reklamy sa nieodlaczna czescia darmowych stron i pozwalaja autorom na ich rozwoj. Jesli doceniasz prace innych, nie uzywaj AdBlocka na tych witrynach.</div>');
} else {
    document.write('<div class="alert alert-danger alert-dismissible" role="alert"><button type="button" class="close" data-dismiss="alert"><span aria-hidden="true">&times;</span><span class="sr-only">Zamknij</span></button><strong>Oj! niedobrze:(</strong> Wyglada na to, ze uzywasz AdBlocka. Reklamy na moim blogu nie sa inwazyjne i w zaden sposob nie przeszkadzaja w odbiorze tresci. Reklamy sa nieodlaczna czescia darmowych stron i pozwalaja autorom na ich rozwoj. Jesli to mozliwe wylacz AdBlocka dla tej domeny. Dziekuje!</div>');
}
