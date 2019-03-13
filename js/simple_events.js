const hello = (name) => {
  console.log(`Hello ${name}!`);
}
const hi = () => {
  console.log('Hi');
}
if (true) {
  hi();
}

setTimeout(hi, 0);

setTimeout(() => hello("Martyna"), 100);

hello("Tomek");

setTimeout(() => hello("Bartek"), 100);

