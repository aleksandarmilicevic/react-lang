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

REACT is embedded in the [Scala](http://scala-lang.org/) programming language.
If you are not familiar with Scala, now is a good time to read some [tutorials](http://docs.scala-lang.org/tutorials/).

A REACT robot extends the `Robot` class in the `react` package.
Each robot has an unique `id`.
This is used to route messages to the right place if you use ROS.

Example
-------

ToDo explain the example (txt, photo of robot, and link to short video)

Here is the code to control that robot:

```scala
import react.Robot
import react.message.Primitive.{Int16,Bool}

class FollowTheEdge(port: String) extends Robot(port) {

  var onTarget = false

  sensor[Bool](sensor){
    case Bool(b) =>
      onTarget = b
  }

  every(100) {
    if (onTarget) { // turning right 
      publish(servoLeft,  Int16(5))
      publish(servoRight, Int16(0))
    } else {        // turning left
      publish(servoLeft,  Int16(0))
      publish(servoRight, Int16(5))
    }
  }

}
```

The controller listen to a sensor that produce a digital/boolean output.
The value is `false` when the sensor is over a black surface and `true` when it is over a white surface.

Every 100 milliseconds, the controller checks the last value it received from the sensor and sets the speed of the servo to turn left or right.
The messages are of type `Int16` as most Arduino are 16-bits micro-controllers.
The name `publish` comes from the interaction of ROS which is a publish-subscribe service.
It is a bit overkill in this example, though.

In the code above, `sensor`, `servoLeft`, and `servoRight` are strings that indicate to which port that sensor and servos are connected.
This is specific to the robot you are using.
Also the value for the (continuous rotation) servos also depends on the hardware.

The full example is at `examples/src/main/scala/react/examples/arduino/FollowTheEdge.scala`.

Running a REACT program is specific to the platform used.
When discussing the different platforms below, we will explain how to run a program for each of them.


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
    ident/pattern => stmnt
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

Control states
--------------

To better structure a program, it is possible to encapsulate event handlers in control states.
The handlers are active only when the program is in that state.
The states are inspired from the _control modes_ in an [hybrid automaton](https://en.wikipedia.org/wiki/Hybrid_automaton).

To use control state, you program must extends `FsmController` on top of `Robot`.
`FsmController` is located in the `react.robot` package.
For example, the declaration of the main class of your robot looks like:

```scala
MyClass(id) extends Robot(id) with FsmController { ...
```

Then states are declared with:

```scala
state( symbol ) {
  // event handlers
  // periodic tasks
  ...
}
```

Each state must have an unique identifier.
[Symbols](http://www.scala-lang.org/api/current/index.html#scala.Symbol) are written as `'name`.

States should contain only event handlers and periodic loops.
It is *not* recommended to declare variables in states but outside any handler.

The program must also declare which state is the initial state using `initialState(symbol)`.

Transitions between states occurs after an handler has executed and called `nextState(symbol)`.
If multiple calls to `nextState` are made during the execution of an handler, the identifier in the last call is used.

ToDo limitations (no parallel composition or nesting for the moment)

Example
-------

ToDo an example with control states (again text, photo, video)

The full example is at `examples/src/main/scala/react/examples/arduino/SwipeScan.scala`.

```scala
import react.Robot
import react.robot.FsmController
import react.message.Primitive.Int16
import react.utils.Env

class SwipeScan(port: String) extends Robot(port) with FsmController {

  val steps = 10
  var stepsLeft = steps

  //about the distance
  var distance = 0

  val servoAngleNA = -200
  val servoAngleInc = 70
  var servoAngle = servoAngleNA

  initialState('scan)

  //always listen to the sensor
  sensor[Int16](sensorDist){
    case PInt16(d) =>
      distance = math.max(d, distance)
  }

  //use the servo that is below the IR sensor to get better picture of the surrounding
  //assume we are stopped (does not set commands to the motors)
  //we leave the servo position is set back to 0
  state('scan) {

    //this is slow as the servo needs to time to move from one position to the other
    every(1000) {
      if (servoAngle == servoAngleNA) {
        distance = 0;  //reset the distance
        servoAngle = -servoAngleInc
        publish(sensorServo, Primitive.Int16(servoAngle.toShort))
      } else if (servoAngle < servoAngleInc) {
        servoAngle += servoAngleInc
        publish(sensorServo, Primitive.Int16(servoAngle.toShort))
      } else {
        publish(sensorServo, Primitive.Int16(0))
        servoAngle = servoAngleNA
        nextState('move)
      }
    }

  }

  
  state('move) {

    every(100) {
      if (stepsLeft == steps) {
        if (distance < safeDistance) { //going straight
          publish(motorLeft,  Int16(6))
          publish(motorRight, Int16(6))
        } else { //turn right
          publish(motorLeft,  Int16(3))
          publish(motorRight, Int16(-3))
        }
        stepsLeft -= 1
      } else if (stepsLeft > 0) {
        stepsLeft -= 1
      } else { //stop
        publish(motorLeft,  Int16(0))
        publish(motorRight, Int16(0))
        stepsLeft = steps
        nextState('scan)
      }
    }

  }

}
```

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
