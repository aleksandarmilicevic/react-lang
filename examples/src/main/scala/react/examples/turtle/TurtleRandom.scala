package react.examples.turtle

import react._
import react.message._
import react.examples._

class TurtleRandom(_id: String) extends Robot(_id) {
  
  //angular/linear scale
  val scaleA = math.Pi / 2 // 90 degree
  val scaleL = 1.0

  //messages for the different actions
  val twistLeft  = Twist(Vector3(0,0,0), Vector3(0,0, scaleA))
  val twistRight = Twist(Vector3(0,0,0), Vector3(0,0,-scaleA))
  val twistUp    = Twist(Vector3( scaleL,0,0), Vector3(0,0,0))
  val twistDown  = Twist(Vector3(-scaleL,0,0), Vector3(0,0,0))
  
  //dispatch a message every second (that's how turtlesim works)
  every(1000){
    math.abs(scala.util.Random.nextInt() % 4) match {
      case 0 => publish("cmd_vel", twistDown)
      case 1 => publish("cmd_vel", twistLeft)
      case 2 => publish("cmd_vel", twistRight)
      case 3 => publish("cmd_vel", twistUp)
      case _ => sys.error("unknown action")
    }
  }

}
