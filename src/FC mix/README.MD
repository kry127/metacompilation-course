# Part I: Flow Chart specialization (mix)

### How to check?

1. To check first projection you need only to launch `Projection 1.rkt` in `drracket` IDE. Just see output :)
2. To check second projection you need to launch `Projection 2 .rkt` in `drracket`.
3. To check third futamura projection you need to launch `Projection 3.rkt` in `drracket`.
4. To see, what happens when you apply `mix (mix mix)` to Flow Chart interpreter, still go to `Projections 3.rkt` and see outputs. This has ben solely done with purpose to utilize intensive-computational `mix (mix mix)` resource. It is recommended to Ctrl+C output and analyze it in your handy editor (because it's long).

### Projection sizes
1. First projecton: *3* basic blocks. Originally: 4 turing commands, 20 basic blocks of tm-int-fc
2. Second projection: *26* basic blocks. Originally: *20* basic blocks of tm-int-fs. Overhead: *+6* blocks
3. Third projection: *37* basic blocks. Originally: 28 blocks of mix.
3. `mix' = (mix mix mix) mix`: *36* basic blocks. Originally: *28* basic blocks of mix. Overhead: *+8* blocks.
   > In my opinion, this is pretty good result!. Also, this `mix'` produces alpha-equivalent program
4. `fc-comp-fc = (mix mix mix) fc-int-fc`: *19* basic blocks. Originally: 20 basic blocks of fc-int-fc. Overhead: *-1* blocks.
4. `fc-int-tm' = fc-comp-fc fc-int-tm`: *28* basic blocks. Originally: 20 basic blocks of tm-int-fc. Overhead: *+8* blocks. Diff with second projection: *+2* blocks!!
   > Pretty printing is needed to get rid of environment variable :))
   
All projections are working correctly
