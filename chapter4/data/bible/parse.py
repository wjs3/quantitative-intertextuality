#!/usr/bin/python

import sys
import os

bibleFile = str(sys.argv[1])

f = open(bibleFile)
bible = f.read().splitlines()
f.close()

book = 1
blankCount = 0
bookStr = '001'

f = open('book_' + bookStr + '.txt', 'w')

for line in bible:

   if len(line.strip()) == 0:
      blankCount += 1
      if blankCount == 4:
         f.close()
         book += 1
         if book > 99:
            bookStr = str(book)
         elif book > 9:
            bookStr = '0' + str(book)
         else:
            bookStr = '00' + str(book)

         f = open('book_' + bookStr + '.txt', 'w')
         
   else:
      blankCount = 0
      f.write(line + '\n') 
