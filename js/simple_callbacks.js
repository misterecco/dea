console.log("start");

function dispatch_callbacks() {
    function inner() {
        function printHello(name) {
            console.log(`Hello {name}`);
        }
        setTimeout(() => printHello("Tomek"), 0);
        setTimeout(printHello, 0);
    }
    inner();
}

dispatch_callbacks();
console.log("end");
