#!/bin/bash

echo getting the husky dependencies ...
wstool init src -j5 https://github.com/husky/install/raw/master/desktop.rosinstall
rosdep install --from-paths src -i -y
catkin_make
