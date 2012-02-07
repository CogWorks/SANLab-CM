#!/bin/bash

BRANCH=`git branch | grep * | awk '{print $2}'`
if [[ "$BRANCH" != "master" ]]
then
    echo "You must switch to the master branch before tagging a revision"
    exit -1
fi

REVISION=`git tag | cut -f 3 -d .`
REVISION=(($REVISION+1))
echo $REVISION > CURRENT_REVISION
git tag -s -m "Release v3.0.$REVISION"
