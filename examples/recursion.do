define factorial (int -- int)
    dup 0 =
    (pop 1)
    (dup 1 - factorial *)
    if
end

#5 factorial print

define fibonacci (int -- int)
    dup 0 =
    (pop 0)
    (dup 1 = 
        (pop 1) 
        (dup dup 1 - swap 2 - fibonacci swap fibonacci + swap pop) 
        if
    )
    if
end

[0 1 2 3 4 5 6 7 8 9 10]
    (fibonacci) map
    sort
    (print) apply