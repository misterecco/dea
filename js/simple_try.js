function throws(n) {
    if (n == 0) {
        throw new Error("wwooooww");
    }
    return n;
}

try {
    console.log(throws(1));
    console.log(throws(0));
} catch (e) {
    console.log(e);
}