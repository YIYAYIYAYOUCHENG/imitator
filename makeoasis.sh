#!/bin/sh

oasis setup && ocaml setup.ml -configure --enable-tests && ocaml setup.ml -all && sh launch.sh
