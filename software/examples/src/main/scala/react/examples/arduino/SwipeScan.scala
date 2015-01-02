package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.utils.Env
import react.examples._

class SwipeScan(port: String) extends Robot(port) with FsmController {

  //port
  val sensorDist   = "33" 
  val sensorServo  = "16"
  val motorLeft    = "27"
  val motorRight   = "4" 

  //constant for the motors
  val defaultSpeed: Short = Env.getShort("speed", 15)
  var lSpeed: Short = Env.getShort("lSpeed", defaultSpeed)
  var rSpeed: Short = Env.getShort("rSpeed", defaultSpeed)

  val lhalf: Short = (lSpeed/2).toShort
  val lmhalf: Short = (-lhalf).toShort
  val rhalf: Short = (rSpeed/2).toShort
  val rmhalf: Short = (-rhalf).toShort

  val steps = 1
  var stepsLeft = steps

  //about the distance
  var distance = 0
  val safeDistance = Env.getInt("safeDistance", 400)
  //TODO min max for the sensor and normalization
  val servoAngleNA = -200
  val servoAngleInc = 70
  var servoAngle = servoAngleNA

  initialState('scan)

  Console.println("safeDistance: " + safeDistance)
  Console.println("lSpeed: " + lSpeed)
  Console.println("rSpeed: " + rSpeed)
  Console.println("lhalf: " + lhalf)
  Console.println("lmhalf: " + lmhalf)
  Console.println("rhalf: " + rhalf)
  Console.println("rmhalf: " + rmhalf)


  //always listen to the sensor
  sensor[Primitive.Int16](sensorDist){
    case Primitive.Int16(d) =>
      //Console.println("distance ← " + d)
      distance = math.max(d, distance)
  }

  //use the servo that is below the IR sensor to get better picture of the surrounding
  //assume we are stopped (does not set commands to the motors)
  //we leaving servo position is set back to 0
  state('scan) {

    every(1000) {
      if (servoAngle == servoAngleNA) {
        distance = 0;
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

    every(1000) {
      if (stepsLeft == steps) {
        if (distance < safeDistance) {
          //Console.println("straight")
          publish(motorLeft, Primitive.Int16(lSpeed))
          publish(motorRight, Primitive.Int16(rSpeed))
        } else {
          //Console.println("right")
          publish(motorLeft, Primitive.Int16(lhalf))
          publish(motorRight, Primitive.Int16(rmhalf))
        }
        stepsLeft -= 1
      } else if (stepsLeft > 0) {
        stepsLeft -= 1
      } else {
        publish(motorLeft, Primitive.Int16(0))
        publish(motorRight, Primitive.Int16(0))
        stepsLeft = steps
        nextState('scan)
      }
    }

  }

}
