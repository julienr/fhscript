function my_function x y {
  while y > 0 {
    y = y - 1;
    x = y + 2;
    print('42');
    while x > 2 {
      print('2');
    }
  }
  return x;
}

function main {
  x = 2 + 3;
  x = my_function(x, 2);
  return 0;
}
