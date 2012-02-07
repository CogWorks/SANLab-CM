#!/bin/bash

LISPWORKS="/Applications/LispWorks 6.1/LispWorks.app/Contents/MacOS/lispworks-6-1-0-macos-universal"

SRC=`pwd`
SANLABCM=/Applications/SANLab-CM/
CONTENTS=${SANLABCM}SANLab-CM.app/Contents/
RESOURCES=${CONTENTS}Resources/

echo "Updating revision info..."
REVISION=`git tag | cut -f 3 -d .`
#REVISION=`svn info http://cwl-projects.cogsci.rpi.edu/svn/SANLab/v3.0/ | grep Revision | cut -d" " -f 2`
echo $REVISION > CURRENT_REVISION

rm -rf ${SANLABCM}
"${LISPWORKS}" -build deliver.lisp
cp -R ${SRC}/Contents/Resources/Template.san ${RESOURCES}

cp ${SRC}/CURRENT_REVISION ${CONTENTS}
cp ${SRC}/Resources/* ${RESOURCES}
cp -R ${SRC}/Activities ${SANLABCM}
cp -R ${SRC}/Distributions ${SANLABCM}
cp -R ${SRC}/docs ${SANLABCM}
cp -R ${SRC}/Interactive\ Routines ${SANLABCM}
cp ${SRC}/properties.conf ${SANLABCM}
cp ${SRC}/COPYING ${SANLABCM}
cp ${SRC}/COPYING.LESSER ${SANLABCM}

IFS="
"
cd /Applications/SANLab-CM/
SVNDIRS=`find . | grep \\.svn`
for dir in $SVNDIRS
do
    rm -rf $dir
done

unset IFS

cd ..
tar cfz ${SRC}/SANLab-CM-OSX-r${REVISION}.tar.gz SANLab-CM
