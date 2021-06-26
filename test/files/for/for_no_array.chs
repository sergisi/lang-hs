f :: () -> Int = fun a {
    for 1 with 0 do fun acc a { acc + a }
};