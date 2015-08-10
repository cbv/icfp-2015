#!/usr/bin/env python
# Main script to run our solution

import argparse
import subprocess
import sys

parser = argparse.ArgumentParser(description='run.py')
parser.add_argument('-m', metavar='N', nargs=1, type=int, help='Memory limit')
parser.add_argument('-f', metavar='s', nargs=1, type=str, help='Memory limit')
parser.add_argument('-c', metavar='N', nargs=1, type=int, help='Memory limit')
parser.add_argument('-t', metavar='N', nargs=1, type=int, help='Memory limit')
parser.add_argument('-p', metavar='N', nargs=1, type=str, help='Memory limit')

args = parser.parse_args(sys.argv[1:])

if args.m is None:
    mem = 1024
else:
    mem = args.m[0]

subprocess.call(["./driver.exe", "@MLton", "fixed-heap", str(mem) + "m", "--"] +
                sys.argv[1:])
