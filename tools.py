import json
import urllib2
import string
import requests
import subprocess
import sys

api = "https://cmage109g3.execute-api.us-west-2.amazonaws.com/what"
scarpy_writer = api + "/scarpydb"

def checkscore(problem, seed, solution):
   call = "./getscore.exe -problem "+str(problem)+\
          " -seed "+str(seed)+" -script '"+solution+"'"
   try:
      res = subprocess.check_output(call,shell=True)
      return json.loads(res)
   except:
      return None

# From a list of tags, generate a tag -> infomap (problem, seed, solution)
def checkdatabase(tags):
   tags = list(set(tags))
   backoff = 50
   moretag = []
   results = {}
   for i in range(0, len(tags)):
      moretag.append({'tag': {'S': tags[i]}})

   while (len(moretag) > 0):
      request = {
         'operation': "batch",
         'RequestItems': {
            'submittydb': {
               'Keys': moretag[:backoff]
            }
         }
      }
      
      response = json.loads(requests.post(api, json.dumps(request)).text)
      try: 
         answers = response['Responses']['submittydb']
         for j in range(0,len(answers)):
            answer = answers[j]
            results[answer['tag']['S']] = {
               'problem': int(answer['problem']['N']),
               'seed': int(answer['seed']['N']),
               'solution': answer['solution']['S']
            }
         print "Status, successfully loaded: "+str(len(moretag[:backoff]))
         moretag = moretag[backoff:]
      except KeyError as e:
         print "failed: "+str(e)
         print json.dumps(response)
         if (backoff > 0): backoff = backoff/2

   return results

def augmentscores(db):
   for tag in db.keys():
      analysis = checkscore(db[tag]['problem'],
                            db[tag]['seed'],
                            db[tag]['solution'])
      if analysis is not None:
         for key in analysis.keys():
            db[tag][key] = analysis[key]
