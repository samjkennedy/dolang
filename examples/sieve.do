define is-multiple? (int int -- bool)
    over over =
    (pop pop false)
    (% 0 =)
    if
end

define swap-2-under (a b c -- a c b)
    rot swap
end

define main (--)
    2 1000 range ???
    reverse dup
    0
    (over over swap len <)              # check if the iterator is less than the length of the list
    (
        dup rot swap nth                #get nth element
        swap-2-under                    #(el lst ind)
        (is-multiple? !) partial filter #(filter out multiples of the element)
        swap                            #(ind lst)
        1 +                             #increment iterator
        over swap                       #(lst ind)
    )
    while
    pop pop (print) apply
end 

main