q :: Bool -> Bool = fun a {
    true && true
};

w :: Bool -> Bool = fun a {
    true || true
};

e :: Bool -> Bool = fun a {
    true == true
};

r :: Bool -> Bool = fun a {
    true != true
};

y :: Bool -> Bool = fun a {
    !true
};
 
qq :: Bool -> Bool -> Bool = fun a b {
    a && b
};

ww :: Bool -> Bool -> Bool = fun a b {
    a || b
};

ee :: Bool -> Bool -> Bool = fun a b {
    a == b
};

rr :: Bool -> Bool -> Bool = fun a b {
    a != b
};

yy :: Bool -> Bool -> Bool = fun a b {
    !a
};

