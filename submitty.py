#!/usr/bin/env python
# Script for submitting and remembering what we submitted

import argparse
import json
import sys
import requests
import socket

from tools import save_get_unique_tag, raw_submitter

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
         host = socket.gethostname()

         unique_tag = save_get_unique_tag(problemId, seed, tag, solution, host)
         raw_submitter(problemId, seed, unique_tag, solution)
   else:
      problemId = args.prob[0]
      seed = args.seed[0]
      tag = "submitty" if args.tag is None else args.tag[0]
      solution = args.sol[0]
      host = socket.gethostname()

      unique_tag = save_get_unique_tag(problemId, seed, tag, solution, host)
      raw_submitter(problemId, seed, unique_tag, solution)
   

   

# json = json.dumps({'problemId': 0, 'seed': 0, 'tag', 'submitty', 'solution': 'Ei!'})
