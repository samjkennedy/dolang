define empty? (a list -- bool)
    len 0 =
end

define not-empty? (a list -- bool)
    empty? !
end

define contains? (a a list -- bool)
    (=) partial map
    (|) false fold
end

define flatten (a list list -- a list)
    [concat] [] fold
end

define square (int -- int)
   dup *
end

define sum (list of int -- int)
    [+] 0 fold
end