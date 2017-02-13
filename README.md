# FSharp-TINY-interpreter
An interpreter of TINY language written in F#
 
# Building

```
fsyacc --module TinyParser TinyParser.fsp
```

Resulting file TinyParser.fs with line 6 as
```
# 0 ""
```
should be commented out.


```
fslex --unicode TinyLex.fsl
```

Further via menu Build in 