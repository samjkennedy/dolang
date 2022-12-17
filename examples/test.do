import "core"

[1 2 3] empty? print
[] empty? print

"------" print

[[2 3] [] [5 6] [] [9 10]] 
    [not-empty?] filter
    flatten
    sort
    reverse
    [print] apply

"------" print

[[1 2] [] [3 4]] 
    [not-empty?] filter
    [dup *] swap flatten swap map 
    [5 >] filter
    sort
    [print] apply