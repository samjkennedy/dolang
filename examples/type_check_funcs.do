import "core"

[1 2 3 4 5]
    [dup *] map
    [+] 0 fold
    print

[true true false]
    [&] true fold
    print

[[3 4] [] [1 2] [] [10] ]
    [not-empty?] filter
    flatten
    [square] map
    sort
    reverse
    [print] apply