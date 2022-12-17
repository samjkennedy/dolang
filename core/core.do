define empty? (list of any -- bool)
    len 0 =
end

define not-empty? (list of any -- bool)
    empty? !
end

define flatten (list of list of any -- list of any)
    [concat] [] fold
end

define square (int -- int)
   dup *
end

define sum (list of int -- int)
    [+] 0 fold
end

define flatmap [[any]] fn([any] -- any) -- [any] in
    swap flatten
    swap map
end