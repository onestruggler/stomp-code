.v 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 
.i 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 
BEGIN
H 3
H 7
H 12
H 2
tof 3 1 2
H 2
H 6
tof 7 5 6
H 6
H 11
tof 12 10 11
H 11
H 3
H 7
H 12
cnot 1 2
cnot 3 4
cnot 5 6
cnot 7 9
cnot 10 11
cnot 12 14
H 7
H 9
cnot 2 3
cnot 11 13
H 6
tof 7 3 6
H 6
H 6
tof 9 4 6
H 6
H 7
H 9
H 12
H 14
H 11
tof 12 7 11
H 11
H 11
tof 14 9 11
H 11
cnot 6 8
cnot 3 6
cnot 7 11
cnot 4 8
cnot 9 13
H 12
H 14
cnot 14 12
cnot 13 11
cnot 8 6
H 14
H 13
H 8
H 12
tof 14 0 12
H 12
H 11
tof 13 0 11
H 11
H 6
tof 8 0 6
H 6
H 14
H 13
H 8
cnot 14 12
cnot 13 11
cnot 8 6
cnot 0 2
END