-- 第二章
add a b = a + b

myDrop n xs = if n <= 0 || null xs 
  then xs 
  else myDrop (n - 1) (tail xs)

lastButOne xs = xs!!(length xs - 2)