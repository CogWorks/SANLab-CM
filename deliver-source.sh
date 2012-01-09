#!/bin/bash

./clean
REVISION=`svn info http://cwl-projects.cogsci.rpi.edu/svn/SANLab/v3.0/ | grep R\
evision | cut -d" " -f 2`
echo $REVISION > CURRENT_REVISION
tar --exclude '\.svn' -c -z -f SANLab-CM-src-r${REVISION}.tar.gz Activities/ Distributions/ Interactive\ Routines/ docs/ Contents/ Resources/ *.lisp COPYING COPYING.LESSER CURRENT_REVISION *.conf deliver*