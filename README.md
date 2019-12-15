# CSE 112 - Comparative Programming Language 
(Former CS112)

## Tiny Language Interpreter in three different versions

The language contains just four types of statements:

**let *variableName* = *expression***

**if *expression* goto *label***

**print *expression1*, *expression2*, ...**

**input *variableName***

Here is a tiny language program that prints out a sequence of numbers:
```
input start
input end
let x = start
repeat: print x
let x = x + 1
if x < end goto repeat
print "that's all", x
```
The program should produce:
```
1.0
2.0
3.0
4.0
that's all 5.0
```

## Flight Schedule in Prolog

The program should produce: 
```
unix3.lt.ucsc.edu117 :swipl prog2.pl
| ?- fly( lax, sea).
depart  LAX  Los Angeles     14:22
arrive  SFO  San Francisco   15:29
depart  SFO  San Francisco   16:02
arrive  SEA  Seattle-Tacoma  17:42
true ?
yes
```
