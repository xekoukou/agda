#!/bin/bash

agda-2.5.2 --ignore-interfaces -v0 --dedukti $1.agda | tee $1.dk
dkcheck $1.dk

# EOF
