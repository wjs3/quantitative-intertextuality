#!/usr/bin/python
#
# HOG feature generator (wrapper for vlfeat library). Assumes images are in pgm
# format and of size 200x200. Generates feature vectors in csv format that can
# be ingested by R.
# 
# Usage: python genFeaturesHOG.py <image paths> <output file>
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

import dsift, sift
import os
from numpy import *
from pylab import *
from PIL import Image

PGM = 1

def hog_histogram(img_list, out_file, debug):

   f = open(img_list)
   filenames = f.read().splitlines()
   f.close()

   image_vector = []
   ctr = 0

   for entry in filenames:

      dsift.process_image_dsift(entry, 'tmp.sift', 20, 10, resize=(200,200))
      l,d = sift.read_features_from_file('tmp.sift')

      if debug:
         im = array(Image.open(entry))
         sift.plot_features(im, l, True)
         show()

      image_vector.append(d.flatten().tolist())
      os.remove('tmp.sift')
      os.remove('tmp.frame')
     
      if not PGM:
         os.remove('tmp.pgm')

      if out_file != None: 
         f = open(out_file, 'a')

         if ctr == 0:
            header = ''
            for i in range(len(image_vector[ctr])):
               if i == 0:
                  header += str(i)
               else:
                  header += ',' + str(i)
            f.write(header)
            f.write('\n')

         vector = ''

         for i in range(len(image_vector[ctr])):
            if i == 0:
               vector += str(image_vector[ctr][i]);
            else:
               vector += ',' + str(image_vector[ctr][i]);

         f.write(vector)
         f.write('\n')

         f.close()

      ctr += 1
     
   return image_vector

if __name__ == '__main__':

    debug = 0

    img_list = sys.argv[1]
    out_file = sys.argv[2]

    hog_histogram(img_list, out_file, debug)
