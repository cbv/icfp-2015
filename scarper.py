#!/usr/bin/env python

import json
import urllib2
    
data = urllib2.urlopen('https://davar.icfpcontest.org/rankings.js')
data = data.read()
start = data.find('{')
data = data[start:]
data = json.loads(data)


# data['time']: string, 2015-08-07 17:08:24.785199 UTC
# data['data']['settings'][n]['setting']: int (= n???)
# data['data']['settings'][n]['rankings'][m]['power_score']: int
# data['data']['settings'][n]['rankings'][m]['tags']: string
# data['data']['settings'][n]['rankings'][m]['rank']: int (<= m+1 ???)
# data['data']['settings'][n]['rankings'][m]['teamId']: int
# data['data']['settings'][n]['rankings'][m]['score']: int
# data['data']['settings'][n]['rankings'][m]['team']: string 

numproblems = len(data['data'])

for i in range(0,numproblems):
   rankings = data['data']['settings'][i]['rankings']
   numteams = len(rankings)
   for j in range(0,numteams):
      if rankings[j]['teamId'] is 31:
          print rankings[j]['team'] + ", problem #" + str(i)
          print "   Ranking:" + str(rankings[j]['rank'])
          print "   Score:" + str(rankings[j]['score'])
          print "   Tags:" + str(rankings[j]['tags'])
          print "   "
