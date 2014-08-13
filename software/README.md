Overview
========

The REACT language for robotics.

Setup
=====

Dependencies
------------

* [ROS indigo](http://wiki.ros.org/)
  Follows the instructions http://wiki.ros.org/indigo/Installation
* [ROS java](http://wiki.ros.org/rosjava)
  Follows the Source Installation instructions at http://wiki.ros.org/rosjava/Tutorials/indigo/Installation.

Initial Setup
-------------
The first time, you will need to run the `setup.sh` script.
Before running the script make sure you have sourced `$ROS_HOME/setup.bash` and `$ROS_JAVA_HOME/devel/setup.bash` in your current shell.
In Ubuntu system, `$ROS_HOME` is usually `/opt/ros/indigo/`.

Directory Structure
-------------------
`react_compiler` and `react_examples` contains the sources of the projects.
The other directories are created/needed by rosjava.
`src` contains a top level `react_lang` projects.
`react_lang` contains two gradle subprojects: `react_compiler` and `react_examples`.
the two sub projects are links that points to the two folder at the root.

Compiling
---------
- `catkin_make` to build the entire wrokspace.
- `cd src/react_lang; ./gradlew build` to rebuild only the java/scala subprojects. (to generate the artifacts, use installApp instead of build)

Running
-------
see Examples

Examples
========
to start the turtlesim teleop example:
- `roscore &`
- `rosrun turtlesim turtlesim_node &`
- `cd src/react_lang/react_examples`
- `./build/install/react_examples/bin/react_examples teleop /turtle1`

Misc
====
For scala syntax highlight in vim, I recommend [https://github.com/derekwyatt/vim-scala.git](https://github.com/derekwyatt/vim-scala.git).
