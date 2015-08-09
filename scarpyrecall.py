#!/usr/bin/env python
from tools import scarpyrecall
db = scarpyrecall()

for problem in sorted(db.keys()):
   print "--------|---------|---------|"+"-"*51
   for i in range(0, len(db[problem])):
      score = str(db[problem][i]['score'])
      power = str(db[problem][i]['power'])
      tags = ""
      print str(problem)+" "*(8-len(str(problem)))+"|",
      print score+" "*(8-len(score))+"|",
      print power+" "*(8-len(power))+"|",
      budget = 50
      for j in range(0, len(db[problem][i]['tags'])):
         new = db[problem][i]['tags'][j]
         if (len(new) > budget):
            print "\n        |         |         |",
            budget = 49
         print new,
         budget = budget - len(new) - 1
      print ""
print ''

