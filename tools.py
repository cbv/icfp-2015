import json
import urllib2
import string
import requests
import subprocess
import sys

# Rob hates web services
api = "https://cmage109g3.execute-api.us-west-2.amazonaws.com/what"
scarpy_writer = api + "/scarpydb"
submitty = api + "/submittydb"

# Rob's api token, don't HAX
apitoken = 'IumEctlsTyz6gaGeix2MQ8wHwnChc2u1roJ7NJpHL20='
contest_server = 'https://davar.icfpcontest.org/teams/31/solutions'

# saves a submission with a unique key in our database
def save_get_unique_tag(problemId, seed, tag, solution, host):
   print "[submitty] Obtaining unique tag..."
   info = {
      'problemId': problemId,
      'seed': seed,
      'tag': tag,
      'solution': solution,
      'submitter': host
   }
   done = False
   while(not done):
      response = json.loads(requests.post(submitty, json.dumps(info)).text)
      if len(response) != 2 or response[0] is None or response[1] is None:
         fail("[submitty] Unexpected output, will retry: "+json.dumps(response))
         time.sleep(3)
      else: done = True

   unique_tag = response[0]
   return unique_tag

# must be called with the UNIQUE tag returned from save_and_get_unique_tag
def raw_submitter(problemId, seed, unique_tag, solution):
   submission = json.dumps([{
      'problemId': problemId,
      'seed': seed,
      'tag': unique_tag,
      'solution': solution
   }])

   print "[submitty] submitting with tag "+unique_tag,
   print "to problem "+str(problemId)+", seed "+str(seed)
   brief = "[submitty] script ("+str(len(solution))+" long) = "+solution
   if len(brief) > 80: brief = brief[:77]+"..."
   print brief

   heads = {'Content-Type':'application/json'}
   print "[submitty] contest server's response, should be 201:",
   response = requests.post(contest_server, 
                            submission, 
                            auth=('',apitoken), 
                            headers=heads)
   print response

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
   print "[load submissions] attempting to load "+str(len(tags))+" tags"
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
         print "[load submissions] successfully loaded: "+str(len(moretag[:backoff]))
         moretag = moretag[backoff:]
      except KeyError as e:
         print "failed: "+str(e)
         print json.dumps(response)
         if (backoff > 0): backoff = backoff/2

   print "[load submissions] done"
   return results

def augmentscores(db):
   for tag in db.keys():
      analysis = checkscore(db[tag]['problem'],
                            db[tag]['seed'],
                            db[tag]['solution'])
      if analysis is not None:
         for key in analysis.keys():
            db[tag][key] = analysis[key]
