define sum (int list -- int)
    (+) 0 fold
end

define avg (int list -- int)
    dup len swap #[Len List]
    sum          #[Len Sum]
    swap         #[Sum Len]
    /            #[Sum/Len]
end

define square (int -- int)
    dup *
end

[6 7 8 9 10] [1 2 3 4 5] 
    concat 
    (square) map 
    avg 
    print

define product (int list -- int)
    (*) 1 fold
end

define sum-all-over-two (int list -- int)
    (2 >) filter sum 
end

define flatten (<T> list list -- <T> list)
    (concat) [] fold
end

[1 2 3 4 5] 
dup sum print 
dup product print
dup sum-all-over-two print
dup 
    (2 >) filter 
    (print) apply
pop

[[1 2 3] [4] [5 6 7] [8 9] [10]] 
    (len 1 >) filter
    (sum) map
    sort
    (print) apply