# Turing Machine in Haskell

## Compile and run


```
ghc turing
```

```
./turing tm-specification.txt tm-tape.txt exresult.txt
```

### File with turing machine specification

First file in command line arguments. 

Example:
```
q0
qk
0 q0 q0 1 Right;1 q0 q0 0 Right;* q0 qk * Still
```
1. Initial state
2. Final states
3. Rules (current tape symbol | current machine state | next machine state | next tape symbol | move direction(Right, Left or Still))



### File with turing machine tape

Second file in command line arguments (symbols seperated by ';')

Example:
```
1;0;0
```

### File with resultant tape

Third file in command line arguments. New tape will be written here.

Example (after executing the program):
```
["0","1","1","__*__"]
```
