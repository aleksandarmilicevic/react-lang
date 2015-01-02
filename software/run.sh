#!/bin/bash
export JAVA_OPTS="-Xms1G -Xmx4G"
source deps-$1.cp
shift
exec java -cp ${cp} ${main} $*
