package react.examples.arduino

import react.Robot
import react.robot.FsmController
import react.message.Primitive.Int16
import react.utils.Env

class SwipeScan2(port: String) extends Robot(port) with FsmController {

  //port
  val motorLeft    = "38"
  val motorRight   = "15" 
  val motorBoth    = "40" 
  val sensorServo  = "27"
  val sensorDist   = "5" 

  //constant for the motors
  val defaultSpeed: Short = Env.getShort("speed", -30)
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
  val safeDistance = Env.getInt("safeDistance", 300)
  val servoAngleNA = -200
  val servoMin = 20
  val servoMax = 120
  val servoMiddle: Short = 70
  val servoAngleInc = 50
  var servoAngle = servoAngleNA

  initialState('scan)

  //always listen to the sensor
  sensor[Int16](sensorDist){
    case Int16(d) =>
      //Console.println("distance ← " + d)
      distance = math.max(d, distance)
  }

  //use the servo that is below the IR sensor to get better picture of the surrounding
  //assume we are stopped (does not set commands to the motors)
  //we leave the servo position is set back to 0
  state('scan) {

    every(1000) {
      if (servoAngle == servoAngleNA) {
        distance = 0;
        servoAngle = servoMin
        publish(sensorServo, Int16(servoAngle.toShort))
      } else if (servoAngle < servoMax) {
        servoAngle += servoAngleInc
        publish(sensorServo, Int16(servoAngle.toShort))
      } else {
        publish(sensorServo, Int16(servoMiddle))
        servoAngle = servoAngleNA
        nextState('move)
      }
    }

  }

  val messageDelay = 50
  
  state('move) {

    every(1000) {
      if (stepsLeft == steps) {
        if (distance < safeDistance) {
          //Console.println("straight")
          publish(motorBoth, Int16(defaultSpeed))
        } else {
          //Console.println("right")
          publish(motorLeft, Int16(lhalf))
          Thread.sleep(messageDelay)
          publish(motorRight, Int16(rmhalf))
        }
        stepsLeft -= 1
      } else if (stepsLeft > 0) {
        stepsLeft -= 1
      } else {
        publish(motorBoth, Int16(0))
        stepsLeft = steps
        nextState('scan)
      }
    }

  }

}
