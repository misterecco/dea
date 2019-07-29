function instrument(obj, withFn) {
    for (const name of Object.keys(obj)) {
        const fn = obj[name];
        if (typeof fn === 'function') {
            obj[name] = (function() {
                return function(...args) {
                    withFn(name, ...args);
                    return fn(...args);
                }
            })();
        }
    }
}

const o = {
    f(w) {
        /* function body */
    }
};

instrument(o, console.log)

o.f("hello");