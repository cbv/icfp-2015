#!/usr/bin/env python

import json
import urllib2
import string
    
api = "https://cmage109g3.execute-api.us-west-2.amazonaws.com/what"
scarpy_writer = api + "/scarpydb"

data = urllib2.urlopen('https://davar.icfpcontest.org/rankings.js')
data = data.read()
start = data.find('{')
data = data[start:]
data = json.loads(data)

# Description of data
# data['time']: string, 2015-08-07 17:08:24.785199 UTC
# data['data']['settings'][n]['setting']: int (= n???)
# data['data']['settings'][n]['rankings'][m]['power_score']: int
# data['data']['settings'][n]['rankings'][m]['tags']: string
# data['data']['settings'][n]['rankings'][m]['rank']: int (<= m+1 ???)
# data['data']['settings'][n]['rankings'][m]['teamId']: int
# data['data']['settings'][n]['rankings'][m]['score']: int
# data['data']['settings'][n]['rankings'][m]['team']: string 

numproblems = len(data['data']['settings'])
scores = []

for i in range(0,numproblems):
   rankings = data['data']['settings'][i]['rankings']
   numteams = len(rankings)
   for j in range(0,numteams):
      if rankings[j]['teamId'] is 31:
          stats = rankings[j]
          if (stats['score'] != 0 or len(stats['tags']) != 0):
             print rankings[j]['team'] + ", problem #" + str(i)
             print "   Ranking: " + str(rankings[j]['rank'])
             print "   Score:   " + str(rankings[j]['score']),
             print "("+str(rankings[j]['power_score'])+" power word(s))"
             print "   Tags:    " + str(rankings[j]['tags'])
             print "   "
             if (len(rankings[j]['tags']) > 0):
                tag = rankings[j]['tags']
                tag.sort()
                newhash = {
                   'tag': string.join(tag, "_"),
                   'problem': i,
                   'score': rankings[j]['score'],
                   'power': rankings[j]['power_score'],
                   'alltags': rankings[j]['tags']
                   }
                scores.append(newhash)

import requests
request = {
   'time': data['time'],
   'scores': scores
}
# print json.dumps(request)
response = json.loads(requests.post(scarpy_writer, json.dumps(request)).text)
if (not 'modified' in response):
   print "Unexpected response!"
   print json.dumps(response)
if (response['modified'] != 0):
   print 'Learned about '+str(response['modified'])+' new scores'
