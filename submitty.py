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
submitty = api + "/submittydb"

def submitter(problemId, seed, tag, solution, host):
   print "Obtaining unique tag..."
   info = {
      'problemId': problemId,
      'seed': seed,
      'tag': tag,
      'solution': solution,
      'submitter': host
   }
   response = json.loads(requests.post(submitty, json.dumps(info)).text)
   if len(response) != 2 or response[0] is None or response[1] is None:
      fail("Unexpected output\n\n"+json.dumps(response))
   unique_tag = response[0]
   attempts = response[1]

   submission = json.dumps([{
      'problemId': problemId,
      'seed': seed,
      'tag': unique_tag,
      'solution': solution
   }])

   print "Submitting with tag "+unique_tag,
   if attempts is 1: print ""
   else: print "("+str(attempts)+" attempts)"

   print "\tProblem:  "+str(problemId)
   print "\tSeed:     "+str(seed)
   print "\tSolution: "+str(solution)
   headers = {'Content-Type':'application/json'}
   print requests.post(url, submission, auth=('',apitoken), headers=headers)
   return unique_tag


def fail(msg):
   print msg
   sys.exit(1)

parser = argparse.ArgumentParser(description='submitty.py')
parser.add_argument('--prob', metavar='N', nargs=1, type=int, help='The number of the problem being solved')
parser.add_argument('--seed', metavar='N', nargs=1, type=int, help='The seed for this problem')
parser.add_argument('--sol', metavar='GAMESTRING', nargs=1, help='The full game solution')
parser.add_argument('--tag', metavar='TAG', nargs=1, help='An optional tag')
parser.add_argument('filename', metavar='JSON_FILENAME', nargs='?', help='An ICFP contest compatible JSON file')

if __name__ == "__main__":
   args = parser.parse_args(sys.argv[1:])
   if args.sol is None and args.prob is None and args.seed is None:
      if args.filename is None:
         print "Either a filename or a problem/seed/tag combo must be given"
         parser.print_help()
         sys.exit(1)
      if args.prob is not None: fail("Both filename and problem given")
      if args.seed is not None: fail("Both filename and seed given")
      if args.sol is not None: fail("Both filename and solution given")
      if args.tag is not None: fail("Both filename and tag given")
      try:
         f = open(args.filename, 'r')
         info = json.loads(f.read())
         results = []
         for i in range(0, len(info)):
            if len(info[i]) != 4: fail("wrong number of elements in hash")
            problemId = info[i]['problemId']
            seed = info[i]['seed']
            tag = info[i]['tag']
            solution = info[i]['solution']
      except: fail("Could not read and parse "+args.filename)

      for i in range(0, len(info)):
         problemId = info[i]['problemId']
         seed = info[i]['seed']
         tag = info[i]['tag']
         solution = info[i]['solution']
         submitter(problemId, seed, tag, solution, socket.gethostname())
   else:
      problemId = args.prob[0]
      seed = args.seed[0]
      tag = "submitty" if args.tag is None else args.tag[0]
      solution = args.sol[0]

   

   

# json = json.dumps({'problemId': 0, 'seed': 0, 'tag', 'submitty', 'solution': 'Ei!'})
