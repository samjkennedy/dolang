define factorial (int -- int)
    dup 0 =
    [pop 1]
    [dup 1 - factorial *]
    if
end

5 factorial print

[0 1 2 3 4 5 6 7 8 9 10] 
    [factorial] map
    sort
    [print] apply