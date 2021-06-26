f :: Int -> Int = fun a {
    repeat fun a { true } until fun a {a < 3} with 0
}