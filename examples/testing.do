define flatten (list of list of any -- list of any)
    [concat] [] fold
end

define sum (list of int -- int)
    [+] 0 fold
end

[[1 2] [1 3]] ???
    [sum] map
    flatten
    [max] 0 fold
    print