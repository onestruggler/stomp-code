.v 0 1 2 3 4 5 6 7 8 9 
.i 0 1 2 3 4 5 6 7 8 9 
BEGIN
H 3
H 6
H 9
H 2
tof 3 1 2
H 2
cnot 1 2
H 2
tof 3 0 2
H 2
H 3
H 5
tof 6 4 5
H 5
cnot 4 5
H 5
tof 6 3 5
H 5
H 6
H 8
tof 9 7 8
H 8
cnot 7 8
H 8
tof 9 6 8
H 8
cnot 6 8
H 9
H 6
H 5
tof 6 3 5
H 5
cnot 4 5
H 5
tof 6 4 5
H 5
cnot 3 5
cnot 4 5
H 6
H 3
H 2
tof 3 0 2
H 2
cnot 1 2
H 2
tof 3 1 2
H 2
cnot 0 2
cnot 1 2
H 3
END