
add :: Int -> Int -> Int = a b { return a + b };

data Point = Point {
     x :: Int,
     y :: Int
};

addPoint :: Point -> Point -> Point = p1 p2 {
  return Point (p1.x + p2.x) (p1.y + p2.y)
};

main :: String[] -> Int = params {
     p = Point 1 2;
     p2 = Point 3 4;
     p3 = addPoint p p2;
     return p3.x + p3.y;
};
