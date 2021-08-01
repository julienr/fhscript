function my_function x y {
  while x > 0 {
    x = x - 1
    call print '42'
  }
  return x
}

function main {
  var x = 2 + 3
  x = call my_function x 2
}
