define empty? (list of any -- bool)
    len 0 =
end

define not-empty? (list of any -- bool) 
    empty? !
end

[1 2 3 4 5 6 7 8 9 10]
[5 >
    [print "Greater than 5" print] 
    [print "Less than or equal to 5" print] 
    if 
] apply
