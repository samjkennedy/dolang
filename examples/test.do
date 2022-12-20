(dup *) [1 2 3 4 5]
    swap map
    (print) apply

[true false false true]
    (!)     map
    (print) apply

[1 2 3 4 5]
    (+) 0 fold
    print

[1 2 3 4 5]
    (*) 1 fold
    print

[true true]
    (&) true fold
    print

[[1 2] [3 4] [5 6]]
    (concat) [] fold
    (+) 0       fold
    print

define sum (list of int -- int)
    (+) 0 fold
end

[1 2 3 4 5] sum print