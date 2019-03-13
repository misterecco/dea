const factorial = ({num}) => {
  if (num <= 1) return 1;
  else return num * factorial({num: num-1});
}

function print_factorial(num, fact) {
  console.log(fact({num}));
}

function hello() {
  console.log("Hello, world!");
}

print_factorial(5, factorial);

hello();
