#!/usr/bin/python

import sys
import os

quranFile = str(sys.argv[1])

f = open(quranFile)
quran = f.read().splitlines()
f.close()

chpt = 1
chptStr = '001'

f = open('chapter_' + chptStr + '.txt', 'w')

for line in quran:
   if line == '________':
      f.close()
      chpt += 1
      if chpt > 99:
         chptStr = str(chpt)
      elif chpt > 9:
         chptStr = '0' + str(chpt)
      else:
         chptStr = '00' + str(chpt)

      f = open('chapter_' + chptStr + '.txt', 'w')
   else:
      f.write(line + '\n') 
