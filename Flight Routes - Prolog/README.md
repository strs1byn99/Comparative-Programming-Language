unix3.lt.ucsc.edu115 :swipl --goal=main -c prog2.pl
unix3.lt.ucsc.edu116 :a.out
|: nyc.
|: lax.
depart  NYC  New York City  09:03
arrive  LAX  Los Angeles    14:22

unix3.lt.ucsc.edu117 :swipl prog2.pl
| ?- fly( lax, sea).
depart  LAX  Los Angeles     14:22
arrive  SFO  San Francisco   15:29
depart  SFO  San Francisco   16:02
arrive  SEA  Seattle-Tacoma  17:42
true ?
yes
