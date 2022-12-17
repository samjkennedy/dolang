define zip #(list of int, list of int -- list of list of int)

end

[3 2 1] [4 5 6]
dup head swap init
rot rot swap 
dup head swap init
rot rot cons
rot rot
dup head swap init
rot rot swap 
dup head swap init
rot rot cons
rot rot
dup head swap init
rot rot swap 
dup head swap init
rot rot cons
rot rot
pop pop 
cons cons
[print] apply