const factorial = ({num}) => {
  if (num <= 1) return 1;
  else return num * factorial({num: num-1});
}

function print_factorial(num) {
  console.log(factorial({num}));
}

print_factorial(4);
