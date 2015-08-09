#!/usr/bin/env python

import json
import urllib2
import string
import requests
import subprocess
import sys
from tools import checkdatabase
from tools import scarpyreport
from tools import augmentscores

api = "https://cmage109g3.execute-api.us-west-2.amazonaws.com/what"
scarpy_writer = api + "/scarpydb"

def checkscore(problem, seed, solution):
   call = "./getscore.exe -problem "+str(problem)+\
          " -seed "+str(seed)+" -script '"+solution+"'"
   res = subprocess.check_output(call,shell=True)
   try:
      return json.loads(res)
   except:
      return None

      

if __name__ == "__main__":
   try:
      data = urllib2.urlopen('https://davar.icfpcontest.org/rankings.js')
      data = data.read()
      start = data.find('{')
      data = data[start:]
      data = json.loads(data)
   except ValueError as e:
      print "Failed to load the contest server's screen so I could ultrascarp it"
      print "Error message: "+str(e)
      sys.exit(1)

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

   tags = []
   for i in range(0,numproblems):
      rankings = data['data']['settings'][i]['rankings']
      for j in range(0,len(rankings)):
         if rankings[j]['teamId'] is 31:
            tags = tags + rankings[j]['tags']

   solutiondb = checkdatabase(tags)
   augmentscores(solutiondb)
   
   for i in range(0,numproblems):
      rankings = data['data']['settings'][i]['rankings']
      numteams = len(rankings)
      for j in range(0,numteams):
         if rankings[j]['teamId'] is 31:
            stats = rankings[j]
            if (stats['score'] != 0 or len(stats['tags']) != 0):
               print rankings[j]['team'] + ", problem #" + str(i)
               print "   Ranking: " + str(rankings[j]['rank'])
               print "   Official score: " + str(rankings[j]['score']),
               print "("+str(rankings[j]['power_score'])+" power word(s))"
               for tag in rankings[j]['tags']:
                  if tag not in solutiondb:
                     print "      Tag: "+tag+" not in database"
                  elif 'score' not in solutiondb[tag]:
                     print "      Tag: "+tag+" had an error when analyzed",
                     print "(script length",
                     print str(len(solutiondb[tag]['solution']))+")"
                  else: 
                     analysis = solutiondb[tag]
                     print "      Tag: "+tag
                     print "      |    Score: "+str(analysis['score'])
                     print "      |    Fate:  "+analysis['fate']
               
               print "   "

   scarpyreport(data['time'], data['data']['settings'])
