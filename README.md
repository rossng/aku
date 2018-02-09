# aku

Build using [Stack](https://docs.haskellstack.org/en/stable/README/)

```
stack setup
stack build
```

`stack test` will run the test suite. 

# Presentation

See `aku-presentation.pdf`.

# aku-debugger

Run `stack exec aku-debugger` to launch the debugger. Commands:

`setm 0 [1,2,3]`: set memory locations 0, 1 and 2 to values 1, 2 and 3 respectively.

`setr X1 5`: set the X1 register to 5

`load "aku-tests/programs/bubble_sort.asm"`: load a program from disk

`reset`: reset the processor to its initial state

`step`: step a single cycle forward

`continue`: step until the processor halts

`goto 5`: set the program counter to 5

`quit`: exit the debugger