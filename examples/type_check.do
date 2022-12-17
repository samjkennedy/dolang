define flatten (seq -- seq)
    [concat] [] fold
end

define empty? (seq -- bool)
    len 0 =
end

define not-empty? (seq -- bool) 
    empty? !
end

[1 2 3] empty? print
[] empty? print

[[1 2] [] [4 5] [6] [7 8] [ ] [10]] [empty?] filter flatten [print] apply


