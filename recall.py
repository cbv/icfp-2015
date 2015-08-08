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
#parser.add_argument('--prob', metavar='N', nargs=1, type=int, help='The number of the problem being solved')
#parser.add_argument('--seed', metavar='N', nargs=1, type=int, help='The seed for this problem')
parser.add_argument('--tag', metavar='TAG', nargs=1, help='An optional tag')

if __name__ == "__main__":
   args = parser.parse_args(sys.argv[1:])
   LIMIT = 80

   if args.tag is not None:
      request = {
         'operation': 'query',
         'TableName': 'submittydb',
         'KeyConditionExpression': 'tag = :value',
         'ExpressionAttributeValues': {':value': {'S': args.tag[0]}}
      }
      response = json.loads(requests.post(api, json.dumps(request)).text)
      try:
         problem = response['Items'][0]['problem']['N'] 
         seed = response['Items'][0]['seed']['N'] 
         solution = response['Items'][0]['solution']['S']
         print "Tag:     "+args.tag[0]
         print "Problem: "+problem
         print "Seed:    "+seed
         print "Solution\n"+'-'*LIMIT
         print solution
      except:
         print "Something went wrong here's a dump of JSON"
         print json.dumps(response)
         sys.exit(1)
      sys.exit(0)
      #except:

   request = {
      'operation': 'list',
      'TableName': 'submittydb'
   }

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
            print "Problem "+str(prob)+" seed "+str(seed)+":"
            for i in range(0, len(db[prob][seed])):
               tag = db[prob][seed][i]['tag']
               solution = db[prob][seed][i]['solution']
               text = "   "+tag+" "*(maxtag + 1 - len(tag))+solution
               if len(text) > LIMIT: text = text[:(LIMIT-3)]+"..."
               print text
            

# json = json.dumps({'problemId': 0, 'seed': 0, 'tag', 'submitty', 'solution': 'Ei!'})
