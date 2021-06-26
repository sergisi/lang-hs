
f :: () -> Int = fun a {
  b :: [Int] = [2, 3, 4];
  c :: Int -> Int -> Int = fun a b { a + b };
  for b with 0 do c
}
