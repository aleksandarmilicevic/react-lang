#!/bin/bash

echo assuming you ran "source /opt/ros/indigo/setup.bash"
echo assuming you ran "source rosjava/devel/setup.bash"

mkdir -p src

########################
echo creating the package for unofficial react messages and the top-level rosjava package for react

cd src

echo react_msgs
catkin_create_pkg react_msgs std_msgs geometry_msgs
cd react_msgs
rm CMakeLists.txt package.xml
ln -s ../../react_msgs/CMakeLists.txt CMakeLists.txt
ln -s ../../react_msgs/package.xml package.xml
ln -s ../../react_msgs/msg/ msg
cd ..

echo react_lang
catkin_create_rosjava_pkg react_lang rosjava_bootstrap rosjava_messages react_msgs
cd react_lang
catkin_create_rosjava_msg_project react_msgs
cd ..

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
echo  compiler
catkin_create_rosjava_project react_verifier
rm -rf react_verifier
ln -s ../../react_verifier react_verifier
echo  examples
catkin_create_rosjava_project react_examples
rm -rf react_examples
ln -s ../../react_examples react_examples
