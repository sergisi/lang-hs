f :: Bool -> Bool = fun a {
    for [1,2,3,4] with 0 do fun acc a { acc + a }
};