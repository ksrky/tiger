# tiger

OCaml implementation of Andrew Appel's "Modern Compiler Implementation in ML".

## Notes

Sample code and test cases can be obtained from [author's website](https://www.cs.princeton.edu/~appel/modern/ml/project.html)

Run the following command to compile runtime.c to MIPS assembly

```
clang -O3 -S -target mips -mcpu=mips32 -stdc=c89 runtime.c
```
