function *factorial(num) {
  let acc = 1;
  while (num > 0) {
    setTimeout(hello);
    acc = multiply(num, acc);
    num--;
    yield acc;
  }
}

function multiply(a, b) {
  return a * b;
}

function hello() {
  console.log('Hello');
}

function print_factorial(num) {
  const gen = factorial(num);

  while (true) {
    const ret = gen.next();
    if (ret.done) break;
    console.log(ret.value);
  }

}

print_factorial(5);
