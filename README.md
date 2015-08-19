Overview
========

The REACT language for robotics.

REACT is a domain specific language / library to simplify the programming of robots and remove much of the boiler-plate code.
The basic building blocks of REACT are finite state machine (for the control structure), periodic controller, and event handlers.

REACT can be used stand-alone or used in conjunction with the Robotic Operating System [ROS](http://www.ros.org/).

REACT can help you get started in your robotic project.
However, it does not aim at replacing a general purpose language and has limitation (especially if low-level fine control is required).

REACT runs on small embedded platform such are the BeagleBone and Raspberry Pi.
However, it does not directly run on _bare metal_ micro-controllers such as Arduino.


Setup
=====

Dependencies
------------

This project requires Java 7.
You can build it using [sbt](http://www.scala-sbt.org/).
To install sbt follow the instructions at [http://www.scala-sbt.org/release/tutorial/Setup.html](http://www.scala-sbt.org/release/tutorial/Setup.html).

Compiling
---------

In a console, execute:
```
$ sbt
> compile
```
After the first compilation you should execute the `generateClassPath.sh` script

Directory Structure
-------------------

`compiler`, `verifier`, and `examples` contains the sources of the project.
`ros-zone` contains a `react_msgs` project for ROS custom messages.

ToDo where to put your own code/project


REACT
=====

ToDo describe the language ...

Event handlers
--------------

* **Periodic events**

  ```scala
  every( int ) {
    stmnt
  }
  ```

* **Sensor events** or subscribing to a ROS topic

  ```scala
  sensor[T]( ident ) {
    ident => stmnt
  }
  ```

* Other events
  
  ToDo explain where they come and how to send them

  ```scala
  on {
    case pattern => stmnt
    ...
  }
    ```

Control stats
-------------

```scala
state( ident ) {
  // event handlers
  // periodic tasks
  ...
}
```

`initialState(ident)` 

`nextState(ident)`

ToDo limitations (no parallel composition for the moment)


Example
-------

ToDo example


REACT and ROS
=============

If you want to interface REACT with ROS and uses custom messages you need:
* [ROS](http://wiki.ros.org/)
  Follows the instructions http://wiki.ros.org/Installation
* [ROS java](http://wiki.ros.org/rosjava)
  Follows the Source Installation instructions at http://wiki.ros.org/rosjava/Tutorials/indigo/Installation.

Setup
-----

In the `ros-zone` folder, you need to run the `setup.sh` script.
Before running the script make sure you have sourced `$ROS_HOME/setup.bash` and `$ROS_JAVA_HOME/devel/setup.bash` in your current shell.
In Ubuntu system, `$ROS_HOME` is usually `/opt/ros/RELEASE/` where `RELEASE` is the version of ROS that you have installed, e.g., jade.


Examples
--------

We are present some examples that show how to interface REACT with ROS.

1. The first example shows how REACT interacts with ROS [turtlesim](http://wiki.ros.org/turtlesim).

  To start the turtlesim teleop example run the following commands:

  - `roscore &`

  - `rosrun turtlesim turtlesim_node &`

  - `./run.sh examples teleop /turtle1`

  The second command should launch the turtlesim with one turtle in its center.
  The third command will launch a REACT program which spawns a small widows.
  (When this window is in focus), it will listen to keyboard events for the directional keys.
  The programs then sends commands to move the turtle (roughly) along a grid.

  The code controlling the robot is `examples/src/main/scala/react/examples/turtle/TurtleTeleop.scala`.

  The code for capturing the key press is `examples/src/main/scala/react/examples/Remote.scala`.
  This file is plain Scala, i.e., it does not contains any REACT specific element.


2. TODO explain what it is/does

  TODO this example is not up to date with ROS Jade

  To run this example:

  - first, you need to install a few dependencies: follow the instructions in `ros-zone/husky/README.md`

  - once you have launched gazebo:
    * `./run.sh examples husky /husky1 &`
    * `./run.sh examples husky /husky2 &`

  Alternatively, instead of the `husky` controller, you can use `huskyG` which tries to move only along the grid axes.


REACT and Arduino
=================

TODO model: central controller that sends commands to an Arduino

TODO description of the protocol (ask Joseph/Ankur)

TODO some example


Verification
============

REACT comes with a built-in model-checker to verify safety properties.
In order to use the model checker, one first need to create a verification scenario.
The scenario specify the dimensions of the environment, any objects, and the robots.

Each robot has two parts:

- A controller (REACT program)

- A physical model of the robot

Furthermore, the system needs to be _closed_, i.e., any interaction with the environment, such as user input, needs to be simulated using a ghost element.

TODO some more explanation

An example of verification scenarios, can be found in: `examples/src/main/scala/react/examples/tests/Verif.scala`
They can be run using `./run.sh examples tests XXX 2>/dev/null` where XXX is the ID of one of the scenario as specified in the source file.


Misc
====

For scala syntax highlight in vim, I recommend [https://github.com/derekwyatt/vim-scala.git](https://github.com/derekwyatt/vim-scala.git).
