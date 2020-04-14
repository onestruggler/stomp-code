.v 0 1 2 3 4 
.i 0 1 2 3 4 
BEGIN
X 4
H 4
cnot 0 2
T 2
Z 2
cnot 0 2
cnot 0 4
cnot 2 4
T 4
cnot 2 4
cnot 0 4
cnot 0 3
cnot 1 3
cnot 2 3
T 3
Z 3
S 3
cnot 2 3
cnot 1 3
cnot 0 3
T 4
S 4
cnot 1 4
cnot 3 4
T 4
cnot 3 4
cnot 1 4
cnot 0 4
cnot 1 4
cnot 2 4
cnot 3 4
T 4
Z 4
S 4
cnot 3 4
cnot 2 4
cnot 1 4
cnot 0 4
cnot 1 3
T 3
Z 3
cnot 1 3
cnot 0 3
cnot 2 3
S 3
cnot 2 3
cnot 0 3
cnot 0 4
cnot 3 4
S 4
cnot 3 4
cnot 0 4
cnot 0 4
cnot 1 4
S 4
cnot 1 4
cnot 0 4
cnot 1 4
cnot 2 4
S 4
cnot 2 4
cnot 1 4
cnot 0 4
Z 4
cnot 0 4
cnot 0 2
cnot 1 2
S 2
cnot 1 2
cnot 0 2
Z 0
S 0
Z 1
S 1
cnot 0 3
cnot 1 3
S 3
cnot 1 3
cnot 0 3
cnot 1 3
cnot 2 3
S 3
cnot 2 3
cnot 1 3
cnot 0 3
Z 3
cnot 0 3
cnot 2 4
S 4
cnot 2 4
cnot 2 3
Z 3
cnot 2 3
cnot 3 4
S 4
cnot 3 4
cnot 1 4
Z 4
cnot 1 4
cnot 1 2
Z 2
cnot 1 2
cnot 0 1
Z 1
cnot 0 1
cnot 2 4
cnot 3 4
S 4
cnot 3 4
cnot 2 4
H 4
cnot 1 4
cnot 0 4
END