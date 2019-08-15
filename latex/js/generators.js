function *factorial(n) {
  let acc = 1;
  while (n > 1) {
    acc = acc * n;
    n = n - 1;
    yield [acc, n];
  }
}

function print_factorial(num) {
  const gen = factorial(num);

  while (true) {
    const ret = gen.next();
    if (ret.done) break;
    const [acc, n] = ret.value;
    console.log(`Acc: ${acc}, n: ${n}`);
  }
}

print_factorial(2);