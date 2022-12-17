define first-and-last(seq -- seq)
    dup head swap
    last cons
end

define print-10 (--)
    10 print
end

[1 2 3] first-and-last [print] apply

print-10

[1 2 3] [4 5 6] concat [print] apply
[1 2 3] 
dup first-and-last [print] apply #build new seq from head and last elements
dup dup init [print] apply
dup tail [print] apply
pop pop 