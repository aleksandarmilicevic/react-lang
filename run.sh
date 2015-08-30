#!/bin/bash
export JAVA_OPTS="-Xms1G -Xmx6G"
source deps-$1.cp
shift
exec java -cp ${cp} ${main} $*
