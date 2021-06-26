f :: Bool -> Bool = fun a {
    for [] with 0 do fun acc a { acc + a }
};