define factorial (int -- int)
    dup 0 =
    [pop 1]
    [dup 1 - factorial *]
    if
end

5 factorial print

0 [dup 10 <] [dup print 1 +] while pop