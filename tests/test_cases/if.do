define empty? (list of any -- bool)
    len 0 =
end

define not-empty? (list of any -- bool) 
    empty? !
end

define len-or-ten (list of any --)
    dup empty? 
    (pop 10 print)
    (len print)
    if
end

[10] len-or-ten
[] len-or-ten
