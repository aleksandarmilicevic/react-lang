package react

abstract class Robot {

  //var position: Vector3D
  //var speed: Vector3D
  //TODO what about the orientation ?

  def on[T](handler: PartialFunction[T, Unit]) = {
    sys.error("TODO")
  }

  def every(period: Int)(body: PartialFunction[Unit, Unit]) = {
    sys.error("TODO")
  }

  //TODO the runtime stuff and the messages ...
  //connections
  //generation of ....

}

//abstract class GroundRobot extends Robot {
//  //we can make sure that z stays 0.
//}

//abstract class FlyingRobot extends Robot {
//
//}
