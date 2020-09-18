# HW01 Turing Machine interpreter on Racket

How to use?
1. Install [Racket](https://github.com/racket)
2. Open file turing-interpreter.rkt via drracket
3. You can enter those lines to test interpreter:
    * Testing with program inlined in code:
    ```
    (turing-machine-interpreter program-example '("1" "1" " " " " "0" " " "0" "0" "1"))
    ```
    * Testing with program hosted in file system:
    ```
    (turing-machine-interpreter (file->lines [full-path-to-file]) '("1" " " "0" "0" "1"))
    ```

There are two programs supplied with the interpreter:
1. `helloworld.tur` -- this one is from [pdf](https://drive.google.com/file/d/1DSg_-astVQF1ftLwcrxwR6NyDblGeU6j/view)
2. `check-left-expansion.tur` -- this program checks for space symbols ' ' generation when moving head to the left
