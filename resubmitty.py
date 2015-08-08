#!/usr/bin/env python

import argparse
import sys
from tools import raw_submitter, checkdatabase

parser = argparse.ArgumentParser(description='submitty.py')
parser.add_argument('tags', metavar='TAG', nargs='+', help='The tags you would like to resubmit to the server...')

if __name__ == "__main__":
   args = parser.parse_args(sys.argv[1:])
   db = checkdatabase(args.tags)
   for tag in db.keys():
      raw_submitter(db[tag]['problem'], db[tag]['seed'], tag, 
                    db[tag]['solution'])
   sys.exit(0)
