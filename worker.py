#!/usr/bin/env python

import requests
import json
import sys
import time
import random
import imp
from submitty import *

api = 'https://cmage109g3.execute-api.us-west-2.amazonaws.com/what'
worker_ready = api+'/worker-bee/ready'
worker_done = api+'/worker-bee/done'

def workfake(worker):
   while(True):
      response = requests.post(worker_ready, json.dumps({'name': worker})).text
      response = json.loads(response)

      if (response['work']):
         group = response['group']
         version = response['version']
         print '*** work, running '+group+" version "+version,

         if (group == 'submitty'):
            info = response['requestinfo']
            print "raw response :" + info
            try:
                info2 = json.loads(info)
                info2 = info2[0]
                tag = submitter(info['problemId'], info['seed'], info['tag'],
                                info['solution'], group)
                response_info = {
                   'tag': {'S': tag}
                }
            except:
               print "It didn't work: "^info

         else:         
            print 'version '+response['version']
            time.sleep(15)
            response_info = {
               'Greeting': {'S': 'hello'},
               'Random': {'S': str(random.random())}
            }

         response = requests.post(worker_done, json.dumps({
            'name': worker,
            'group': group,
            'version': version,
            'result': response_info
         }))
      else:
         print '*** no work, sleeping for '+str(response['wait'])
         time.sleep(response['wait'])
 
if __name__ == "__main__":
   if not (len(sys.argv) == 2):
      print "Usage: ./fake-worker.py WORKERNAME"
      sys.exit(1)
   workfake(sys.argv[1])
