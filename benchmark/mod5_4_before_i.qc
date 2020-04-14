.v 0 1 2 3 4 
.i 0 1 2 3 4 
BEGIN
X 4
H 4
H 3
tof 4 0 3
H 3
H 3
tof 4 2 3
H 3
H 4
cnot 3 4
H 4
H 2
tof 4 1 2
H 2
H 4
cnot 2 4
H 4
H 1
tof 4 0 1
H 1
H 4
cnot 1 4
cnot 0 4
END