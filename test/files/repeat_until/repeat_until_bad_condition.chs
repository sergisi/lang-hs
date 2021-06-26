f :: Int -> Int = fun a {
    repeat fun a { a+1 } until fun a {a + 3} with 0
}