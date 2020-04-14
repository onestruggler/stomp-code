.v 0 1 2 3 4 5 6 7 8 9 10 11 12 13 
.i 0 1 2 3 4 5 6 7 8 9 10 11 12 13 
BEGIN
cnot 4 3
cnot 6 5
cnot 8 7
cnot 10 9
cnot 12 11
cnot 4 2
H 2
H 1
tof 2 0 1
H 1
H 2
cnot 6 4
H 4
H 3
tof 4 2 3
H 3
H 4
cnot 8 6
H 6
H 5
tof 6 4 5
H 5
H 6
cnot 10 8
H 8
H 7
tof 8 6 7
H 7
H 8
cnot 12 10
H 10
H 9
tof 10 8 9
H 9
H 10
cnot 12 13
X 3
X 5
X 7
X 9
H 13
H 11
tof 13 10 11
H 11
H 13
cnot 2 3
cnot 4 5
cnot 6 7
cnot 8 9
cnot 10 11
H 10
H 9
tof 10 8 9
H 9
H 10
H 8
H 7
tof 8 6 7
H 7
H 8
X 9
cnot 12 10
H 6
H 5
tof 6 4 5
H 5
H 6
X 7
cnot 10 8
H 4
H 3
tof 4 2 3
H 3
H 4
X 5
cnot 8 6
H 2
H 1
tof 2 0 1
H 1
H 2
X 3
cnot 6 4
cnot 4 2
cnot 1 0
cnot 4 3
cnot 6 5
cnot 8 7
cnot 10 9
cnot 12 11
END