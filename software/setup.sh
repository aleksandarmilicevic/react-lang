#!/bin/bash

echo assuming you ran "source /opt/ros/indigo/setup.bash"
echo assuming you ran "source rosjava/devel/setup.bash"

########################
echo creating the top-level rosjava package for react

mkdir -p src

cd src
catkin_create_rosjava_pkg react_lang
cd ..
catkin_make

source devel/setup.bash

echo don\'t forget to run "source devel/setup.bash" before doing anything

echo
echo

######################
echo creating the projects within the java package
cd src/react_lang
echo  compiler
catkin_create_rosjava_project react_compiler
rm -rf react_compiler
ln -s ../../react_compiler react_compiler
echo  examples
catkin_create_rosjava_project react_examples
rm -rf react_examples
ln -s ../../react_examples react_examples
