#!/usr/bin/python

import sys

rawTextFile = sys.argv[1]

f = open(rawTextFile)
rawText = f.read().splitlines()
f.close()

latinTable = dict()

for line in rawText:
   entry = line.split(",")
   if entry[0] in latinTable:
      currEntries = latinTable[entry[0]].split("-")
      found = 0
      for currEntry in currEntries:
         if entry[1] == currEntry:
            found = 1
            break
  
      if found != 1: 
         newStr = latinTable[entry[0]] + '-' + entry[1]
         latinTable[entry[0]] = newStr 
   else:
      latinTable[entry[0]] = entry[1]

for key in latinTable.keys():
   print key + ',' + latinTable[key]
