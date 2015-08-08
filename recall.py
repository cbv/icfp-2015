#!/usr/bin/env python
# Script for submitting and remembering what we submitted

import argparse
import json
import sys
import requests
import socket

# Rob's api token, don't HAX
apitoken = 'IumEctlsTyz6gaGeix2MQ8wHwnChc2u1roJ7NJpHL20='

url = 'https://davar.icfpcontest.org/teams/31/solutions'
api = "https://cmage109g3.execute-api.us-west-2.amazonaws.com/what"

def fail(msg):
   print msg
   sys.exit(1)

parser = argparse.ArgumentParser(description='submitty.py')
parser.add_argument('--prob', metavar='N', nargs=1, type=int, help='The number of the problem being solved')
parser.add_argument('--seed', metavar='N', nargs=1, type=int, help='The seed for this problem')
parser.add_argument('--tag', metavar='TAG', nargs=1, help='An optional tag')

if __name__ == "__main__":
   request = {
      'operation': 'list',
      'TableName': 'submittydb'
   }
   LIMIT = 80

   response = json.loads(requests.post(api, json.dumps(request)).text)
   info = response['Items']
   db = {}
   for i in range(0, len(info)):
      problem = int(info[i]['problem']['N'])
      #submitter = info[i]['submitter']['S']
      seed = int(info[i]['seed']['N'])
      tag = info[i]['tag']['S']
      solution = info[i]['solution']['S']
      if problem not in db.keys(): db[problem] = {}
      if seed not in db[problem].keys(): db[problem][seed] = []
      db[problem][seed].append({'tag': tag, 'solution': solution})

   sorted(db, key = db.get)
   maxtag = 0
   for prob in db.keys():
      if db[prob] is not None:
         for seed in db[prob].keys():
            for i in range(0, len(db[prob][seed])):
               maxtag = max(maxtag, len(db[prob][seed][i]['tag']))

   for prob in db.keys():
      if db[prob] is not None:
         for seed in db[prob].keys():
            for i in range(0, len(db[prob][seed])):
               print "Problem "+str(prob)+" seed "+str(prob)+":"
               tag = db[prob][seed][i]['tag']
               solution = db[prob][seed][i]['solution']
               text = "   "+tag+" "*(maxtag + 1 - len(tag))+solution
               if len(text) > LIMIT: text = text[:(LIMIT-3)]+"..."
               print text
            

# json = json.dumps({'problemId': 0, 'seed': 0, 'tag', 'submitty', 'solution': 'Ei!'})
