function *factorial(num) {
  let acc = 1;
  while (num > 0) {
    setTimeout(hello);
    acc = num * acc;
    num--;
    yield acc;
  }
}

function hello() {
  console.log('Hello');
}

function print_factorial(num) {
  const gen = factorial(num);

  while (true) {
    const ret = gen.next();
    console.log(ret.value);
    if (ret.done) break;
  }

}

print_factorial(5);
