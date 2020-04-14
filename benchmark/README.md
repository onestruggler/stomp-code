# fast-stomp
input and output benchmark circuits of fast-stomp

Feynman (https://github.com/meamy/feynman) has verified all the circuits except {gf64, gf128, gf131, gf163, gf256,gf512} which are too large to verify. 


The file fastbm.log contains final t-count and other statistics.

This folder contains 3 kinds of files:

   1. the one with extension .log (except fastbm.log) is the proof output by "feynver".

   2. the one with _i is the input circuit in .qc format.

   3. the one with _o is the output circuit of fast-stomp.

Also, the pair of _i and _o file is the input of feynver.

# 

All the circuits except gf256.tfc and gf512.tfc are from: https://github.com/njross/optimizer/tree/master/QFT_and_Adders
gf256.tfc and gf512.tfc are downloaded from: http://webhome.cs.uvic.ca/~dmaslov/

The optimized gf512 is of size 204.5MB, too large to upload. If need, please contact: bian@dal.ca 

Thank Matt for timely adding new functionality in Feynman.
