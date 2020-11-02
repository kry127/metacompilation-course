# Part I: Flow Chart specialization (mix)

### How to check?

In all points below you just need to see outputs of the program

Projections applied to **FC-INT-TM**:
1. To check first projection you need only to launch `projection_1.rkt` in `drracket` IDE. Just see output :)
2. To check second projection you need to launch `projection_2 .rkt` in `drracket`.
3. To check third futamura projection you need to launch `projection_3.rkt` in `drracket`.

Then, those projections were applied to **FC-INT-FC**:
1. To check first projection you need only to launch `fc_rojection_1.rkt` in `drracket`.
2. To check second projection you need to launch `fc_projection_2.rkt` in `drracket`.
3. To check third futamura projection you need to launch `fc_projection_3.rkt` in `drracket`.

### Projection sizes
1. First projecton: *4* basic blocks. Originally: 4 turing commands, 20 basic blocks of tm-int-fc
2. Second projection: *27* basic blocks. Originally: *20* basic blocks of tm-int-fs. Overhead: *+7* blocks
3. Third projection: *38* basic blocks. Originally: 28 blocks of mix.
3. `mix' = (mix mix mix) mix`: *37* basic blocks. Originally: *28* basic blocks of mix. Overhead: *+9* blocks.
   > In my opinion, this is pretty good result!. Also, this `mix'` produces alpha-equivalent program
4. `fc-comp-fc = (mix mix mix) fc-int-fc`: *20* basic blocks. Originally: 20 basic blocks of fc-int-fc. Overhead: *0* blocks.
4. `fc-int-tm' = fc-comp-fc fc-int-tm`: *29* basic blocks. Originally: 20 basic blocks of tm-int-fc. Overhead: *+9* blocks. Diff with second projection: *+2* blocks!!
   > Pretty printing is needed to get rid of environment variable :))
   
All projections are working correctly
