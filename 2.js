function my_function x y {
  while x > 0 {
    x = x - 1;
    call print '42';
    while x > 2 {
      call print '2';
    }
  }
  return x;
}

function main {
  x = 2 + 3;
  x = call my_function x 2;
  return 0;
}
