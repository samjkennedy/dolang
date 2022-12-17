define sum (list of int -- int)
    [+] 0 fold
end

define flatten (list of list of any -- list of any)
    [concat] [] fold
end

[[1000 2000 3000] [4000] [5000 6000] [7000 8000 9000][10000]]
    [sum] map
    flatten
    [max] 0 fold
    print