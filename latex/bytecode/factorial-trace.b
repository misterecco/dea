[generated bytecode for function: factorial]
Parameter count 2
Register count 3
Frame size 24
         0x315c6429eb32 @    0 : 61 a7 01 fb 00    CallRuntime [TraceEnter], r0-r0
   18 E> 0x315c6429eb37 @    5 : a5                StackCheck
   28 S> 0x315c6429eb38 @    6 : 0c 01             LdaSmi [1]
   37 E> 0x315c6429eb3a @    8 : 6b 02 00          TestLessThanOrEqual a0, [0]
         0x315c6429eb3d @   11 : 99 06             JumpIfFalse [6] (0x315c6429eb43 @ 17)
         0x315c6429eb3f @   13 : 0c 01             LdaSmi [1]
         0x315c6429eb41 @   15 : 8b 15             Jump [21] (0x315c6429eb56 @ 36)
   48 E> 0x315c6429eb43 @   17 : 13 00 02          LdaGlobal [0], [2]
         0x315c6429eb46 @   20 : 26 fa             Star r1
         0x315c6429eb48 @   22 : 25 02             Ldar a0
   64 E> 0x315c6429eb4a @   24 : 41 01 04          SubSmi [1], [4]
         0x315c6429eb4d @   27 : 26 f9             Star r2
   52 E> 0x315c6429eb4f @   29 : 5d fa f9 05       CallUndefinedReceiver1 r1, r2, [5]
   50 E> 0x315c6429eb53 @   33 : 36 02 01          Mul a0, [1]
         0x315c6429eb56 @   36 : 26 fb             Star r0
         0x315c6429eb58 @   38 : 61 a8 01 fb 01    CallRuntime [TraceExit], r0-r0
   69 S> 0x315c6429eb5d @   43 : a9                Return
Constant pool (size = 1)
0x315c6429eae1: [FixedArray] in OldSpace
 - map: 0x3806c0d807b1 <Map>
 - length: 1
           0: 0x315c6429e741 <String[#9]: factorial>
Handler Table (size = 0)