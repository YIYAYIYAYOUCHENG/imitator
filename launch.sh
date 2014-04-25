#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Small script to build IMITATOR
 #
 # Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 # 
 # Author:        Etienne Andre
 # 
 # Created:       2014/01/13
 # Last modified: 2014/01/13
 #
################################################################

mpiexec -n 4 bin/PaTATOR
