q :: () -> Bool = fun a {
    'c' == 'c'
};

w :: () -> Bool = fun a {
    'c' != 'c'
};

e :: () -> Bool = fun a {
    'c' > 'c'
};

r :: () -> Bool = fun a {
    'c' < 'c'
};

t :: () -> Bool = fun a {
    'c' <= 'c'
};

t :: () -> Bool = fun a {
    'c' >= 'c'
};

qq :: Char -> Char -> Bool = fun a b {
    a == b
};

ww :: Char -> Char -> Bool = fun a b {
    a != b
};

ee :: Char -> Char -> Bool = fun a b {
    a > b
};

rr :: Char -> Char -> Bool = fun a b {
    a < b
};

tt :: Char -> Char -> Bool = fun a b {
    a >= b
};

yy :: Char -> Char -> Bool = fun a b {
    a <= b
};