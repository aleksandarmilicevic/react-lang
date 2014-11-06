package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.examples._
import react.runtime.Arduino
import react.runtime.ArduinoExecutor

class ArduTest(port: String) extends Robot(port) {

  val led = "0" //Arduino.D13
  val switch = Arduino.D2
  val ir = "1" //Arduino.A2
  val servo = Arduino.D9

  //input from keyboard
  on {
    case Key.UP =>   
      Console.println("led → true")
      publish(led, Primitive.Bool(true))
    case Key.DOWN =>
      Console.println("led → false")
      publish(led, Primitive.Bool(false)) 
  }

  var distance = 0
  sensor[Primitive.Int16](ir){
    case Primitive.Int16(d) =>
      Console.println("distance ← " + d)
      distance = d
  }

//var switched = false
//sensor[Primitive.Bool](switch){
//  case Primitive.Bool(b) =>
//    Console.println("switch ← " + b)
//    switched = b
//}

  every(1000) {
    //if (switched) {
    //Console.println("servo → " + distance)
    //publish(servo, Primitive.Int16(distance.toShort))
    //}
  }

}

object Run {

  def apply(port: String) {
    Console.println("arduino test port: " + port)
    val r = new ArduTest(port)
    val exec = new ArduinoExecutor(r, false, Some(100))
    //val exec = new ArduinoExecutor(r)
    new Remote(r)
    exec.start
  }

}
