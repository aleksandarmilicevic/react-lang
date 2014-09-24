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
`react_compiler`, `react_verifier`, and `react_examples` contains the sources of the project.
The other directories are created/needed by rosjava.
`src` contains a top level `react_lang` projects.
`react_lang` contains three gradle subprojects for the compiler, verifier, and examples.
the three sub projects are links that points to the corresponding folder at the root.

Compiling
---------
- `catkin_make` to build the entire workspace.
- `cd src/react_lang; ./gradlew installApp` to rebuild only the java/scala subprojects.

Examples
========
to start the turtlesim teleop example:
- `roscore &`
- `rosrun turtlesim turtlesim_node &`
- `cd src/react_lang/react_examples`
- `./build/install/react_examples/bin/react_examples teleop /turtle1`

for the husky example:
- first, you need to install a few dependencies: follow the steps in `react_examples/src/main/resources/husky/`
- once you have launched gazebo:
  * `cd src/react_lang/react_examples`
  * `./build/install/react_examples/bin/react_examples husky /husky1 &`
  * `./build/install/react_examples/bin/react_examples husky /husky2 &`
Alternatively, instead of the `husky` controller, you can use `huskyG` which tries to move only along the grid axes.

Verification
============
REACT comes with a built-in model-checker to verify safety properties.
In order to use the model checker, one first need to create a verification scenario.
The scenario specify the dimensions of the environment, any objects, and the robots.
Each robot has two parts:
- the controller (a REACT program)
- a physical model
Furthermore, the system needs to be closed.
Any interaction with the environment, such as user input, needs to be simulated using a ghost element.

An example of verification scenarios, can be found in: `react_examples/src/main/scala/react/examples/tests/Verif.scala`
They can be run using `./react_examples/build/install/react_examples/bin/react_examples tests XXX 2>/dev/null` where XXX is the ID of one of the scenario as specified in the source file.

Misc
====
To give more memory to the JVM: `export JAVA_OPTS="-Xms1G -Xmx4G"`
For scala syntax highlight in vim, I recommend [https://github.com/derekwyatt/vim-scala.git](https://github.com/derekwyatt/vim-scala.git).
