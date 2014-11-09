package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.examples._

class SwipeScan(port: String) extends Robot(port) with FsmController {

  //port
  val sensorDist   = "16" 
  val sensorServo  = "20"
  val motorLeft    = "10"
  val motorRight   = "4" 

  //constant for the motors
  val speed: Short = 80
  val half: Short = (speed/2).toShort
  val mhalf: Short = (-half).toShort
  val steps = 2
  var stepsLeft = steps

  //about the distance
  var distance = 0
  val safeDistance = 150
  //TODO min max for the sensor and normalization
  val servoAngleNA = -200
  val servoAngleInc = 70
  var servoAngle = servoAngleNA

  initialState('scan)


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
          publish(motorLeft, Primitive.Int16(speed))
          publish(motorRight, Primitive.Int16(speed))
        } else {
          //Console.println("right")
          publish(motorLeft, Primitive.Int16(half))
          publish(motorRight, Primitive.Int16(mhalf))
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
