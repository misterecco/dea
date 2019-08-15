function dispatch_callbacks() {
    function inner() {
        function printHello(name) {
            console.log(`Hello, ${name}`);
        }
        printHello("Alice");
        setTimeout(() => printHello("World"), 0);
        setTimeout(printHello, 0);
    }
    inner();
}

console.log("start");
dispatch_callbacks();
console.log("end");