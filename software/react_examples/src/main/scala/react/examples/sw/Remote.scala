package react.examples.sg


object Key extends Enumeration {
  type Key = Value
  val UP, DOWN, LEFT, RIGHT, NONE = Value
}

//TODO a small app to control a robot:
// -ROS way: push messages to a specific topic
// -common way: receive the robot as argument and call methods of the robot ...
