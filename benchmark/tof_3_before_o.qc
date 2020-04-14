.v 0 1 2 3 4 5 6 
.i 0 1 2 3 4 
BEGIN
H 4
H 3
H 5
H 6
cnot 0 3
cnot 2 3
T 3
cnot 2 3
cnot 0 3
cnot 0 2
T 2
Z 2
S 2
cnot 0 2
cnot 0 6
cnot 1 6
cnot 4 6
T 6
Z 6
S 6
cnot 4 6
cnot 1 6
cnot 0 6
cnot 0 5
T 5
Z 5
S 5
cnot 0 5
cnot 0 6
cnot 4 6
T 6
cnot 4 6
cnot 0 6
cnot 0 5
cnot 3 5
T 5
cnot 3 5
cnot 0 5
cnot 0 5
cnot 2 5
T 5
cnot 2 5
cnot 0 5
cnot 0 5
cnot 2 5
cnot 3 5
T 5
Z 5
S 5
cnot 3 5
cnot 2 5
cnot 0 5
T 1
cnot 0 3
T 3
Z 3
S 3
cnot 0 3
cnot 4 6
T 6
Z 6
S 6
cnot 4 6
cnot 0 1
T 1
Z 1
S 1
cnot 0 1
cnot 1 6
cnot 4 6
T 6
cnot 4 6
cnot 1 6
cnot 1 6
Z 6
S 6
cnot 1 6
cnot 0 4
cnot 1 4
S 4
cnot 1 4
cnot 0 4
cnot 0 4
Z 4
S 4
cnot 0 4
cnot 0 6
Z 6
S 6
cnot 0 6
cnot 4 5
Z 5
S 5
cnot 4 5
Z 0
cnot 5 6
Z 6
S 6
cnot 5 6
S 1
cnot 0 6
cnot 1 6
S 6
cnot 1 6
cnot 0 6
cnot 2 3
Z 3
S 3
cnot 2 3
cnot 2 5
cnot 3 5
S 5
cnot 3 5
cnot 2 5
Z 4
cnot 3 5
Z 5
S 5
cnot 3 5
Z 5
S 5
cnot 1 4
Z 4
S 4
cnot 1 4
S 2
S 3
cnot 0 1
Z 1
S 1
cnot 0 1
Z 6
cnot 2 5
Z 5
S 5
cnot 2 5
swap 5 4
swap 6 4
H 6
H 5
H 3
H 4
END