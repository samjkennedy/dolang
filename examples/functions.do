define sum (list of int -- int)
    [+] 0 fold
end

define avg (list of int -- int)
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
    [square] map 
    avg 
    print

define product (list of int -- int)
    [*] 1 fold
end

define sum-all-over-two (list of int -- int)
    [2 >] filter sum 
end

define flatten (list of list of any -- list of any)
    [concat] [] fold
end

[1 2 3 4 5] 
dup sum print 
dup product print

dup sum-all-over-two print

dup [2 >] filter [print] apply

[[1 2] [3 4] [5 6 7] [8 9 10]] 
    [len 3 =] filter
    [sum] map
    [sort] map
    flatten
    [print] apply
pop