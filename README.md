# stomp-code
Haskell implementation of the T-count optimization algorithm "Stomp"

# instruction to run 
 1. install Haskell platform
 2. install Quipper
 3. compile Main using "ghc -O2 Main"
 4. create "Stomp" folder(in the same folder where you executable Main is)
 5. run "./Main -tcount A.log -identity 45 -order wire B", where A is your log file name, B is your circuit. we support "tfc" format classical reversible circuit (.tfc extension), and Quipper circuit format (empty extension or any extension). 
 6. relevant statistics will be in A.log file. The optimized circuit will be in .qc format and in the Stomp folder, together with the input circuit in .qc format. 
 7. the optimized and input .qc circuits can be piped to "feynver" for verification.
 
# ** Disclaimer

This code runs, and is the basis of our results, but is not in an ideal state; this is to be addressed in future revisions.

