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

oasis setup && ocaml setup.ml -configure --enable-tests && ocaml setup.ml -all && mpiexec -n 4 bin/IMITATOR ex/flipflop.imi ex/flipflop.v0 -mode cover -distributed -cart -verbose medium
