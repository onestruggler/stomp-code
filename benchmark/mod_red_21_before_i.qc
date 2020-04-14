.v 0 1 2 3 4 5 6 7 8 9 10 
.i 0 1 2 3 4 5 6 7 8 9 10 
BEGIN
H 4
H 3
tof 4 1 3
H 3
H 4
X 4
X 5
H 6
H 5
tof 6 4 5
H 5
H 6
X 5
X 4
X 6
H 9
H 7
tof 9 6 7
H 7
H 9
X 6
X 4
X 5
H 6
H 8
H 5
tof 6 4 5
H 5
H 10
tof 8 9 10
H 10
H 8
X 5
X 4
H 4
H 2
H 3
tof 4 1 3
H 3
cnot 8 0
cnot 8 1
cnot 8 5
H 1
tof 2 0 1
H 1
H 2
X 2
H 3
tof 4 2 3
H 3
H 4
X 2
X 4
H 5
tof 6 4 5
H 5
H 6
X 4
X 6
H 7
H 8
tof 7 6 8
H 8
H 7
X 6
X 4
H 6
H 5
tof 6 4 5
H 5
H 6
X 4
H 5
H 8
tof 5 4 8
H 8
H 5
X 2
H 4
H 3
tof 4 2 3
H 3
H 4
X 2
X 2
H 3
H 8
tof 3 2 8
H 8
H 3
X 2
H 2
H 1
tof 2 0 1
H 1
H 2
X 0
H 1
H 8
tof 1 0 8
H 8
H 1
X 0
H 8
H 10
tof 8 9 10
H 10
H 8
END