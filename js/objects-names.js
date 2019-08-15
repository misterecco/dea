var Service = {};

Service.hello = function(name) {
    console.log(`Hello, ${name}`);
}

function hello() {
    Service.hello('Yo');
}

hello();