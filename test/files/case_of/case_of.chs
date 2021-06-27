f :: () -> Int = fun a {
    data Person = Small | Medium | Tall;
    g :: Person -> Int = fun a {
        case a of 
        Small { 1 },
        Medium { 2 },
        Tall { 3 }
    };
    g Small
}