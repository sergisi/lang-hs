f :: Int -> Int -> Int = fun a b {
  d :: Int = a + b;
  c :: Int = d * d + a - b;
  c + d
};

g :: Int -> Int = fun a {
  d :: Int = f a a;
  c :: Int = f d d;
  c + c
};
