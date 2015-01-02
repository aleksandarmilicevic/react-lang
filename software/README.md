Overview
========

The REACT language for robotics.

Setup
=====

Dependencies
------------

This project requires java 7.
You can build it using [sbt](http://www.scala-sbt.org/).
To install sbt follow the instructions at [http://www.scala-sbt.org/release/tutorial/Setup.html](http://www.scala-sbt.org/release/tutorial/Setup.html).

If you want to interface REACT with ROS and uses custom messages you need:
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
`compiler`, `verifier`, and `examples` contains the sources of the project.
`ros-zone` contains a `react_msgs` project for ROS custom messages.

Compiling
---------
In a console, execute:
```
$ sbt
> compile
```
After the first compilation you should execute the `generateClassPath.sh` script


Examples
========
to start the ROS turtlesim teleop example:
- `roscore &`
- `rosrun turtlesim turtlesim_node &`
- `./run.sh examples teleop /turtle1`

for the husky example:
- first, you need to install a few dependencies: follow the steps in `ros-zone/husky/`
- once you have launched gazebo:
  * `./run.sh examples husky /husky1 &`
  * `./run.sh examples husky /husky2 &`
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
They can be run using `./run.sh examples tests XXX 2>/dev/null` where XXX is the ID of one of the scenario as specified in the source file.

Misc
====
For scala syntax highlight in vim, I recommend [https://github.com/derekwyatt/vim-scala.git](https://github.com/derekwyatt/vim-scala.git).
