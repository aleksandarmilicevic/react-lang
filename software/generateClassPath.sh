#!/bin/bash

function mkCP {
    file=deps-$1.cp
    lib=`pwd`/lib/\*
    echo -n "cp=\"$lib:" > $file
    sbt "show react-$1/dependencyClasspath" | tr -d ' ' | tr ',' '\n' | gawk 'match($0, /Attributed\(([^)]*)\)/, a) {print a[1]}' | tr '\n' ':' >> $file
    echo -n "$1/target/scala-2.11/classes" >> $file
    echo '"' >> $file
    echo -n "main=\"" >> $file
    sbt "show react-$1/mainClass" | gawk 'match($0, /Some\(([^)]*)\)/, a) {print a[1]}' | tr -d '\n' >> $file
    echo '"' >> $file
}

mkCP compiler

mkCP verifier

mkCP examples
