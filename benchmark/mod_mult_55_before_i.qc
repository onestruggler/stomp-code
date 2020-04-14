.v 0 1 2 3 4 5 6 7 8 
.i 0 1 2 3 4 5 6 7 8 
BEGIN
X 2
H 7
H 2
tof 7 0 2
H 2
H 7
X 2
X 0
H 6
H 8
H 7
tof 6 1 7
H 7
H 2
tof 8 0 2
H 2
H 8
H 6
cnot 6 5
cnot 6 3
cnot 8 7
X 1
X 7
H 3
H 6
H 8
tof 6 1 8
H 8
H 7
tof 3 1 7
H 7
H 6
H 3
X 1
cnot 6 4
cnot 5 8
H 5
H 8
H 7
tof 5 1 7
H 7
H 2
tof 8 0 2
H 2
H 8
H 5
X 0
X 7
cnot 5 8
END