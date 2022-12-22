define contains? (int int list -- bool) 
    (=) partial map
    (|) false fold
end

[[1 2] [2 3] [3 4]]
    (2 contains?) filter
    ((print) apply) apply