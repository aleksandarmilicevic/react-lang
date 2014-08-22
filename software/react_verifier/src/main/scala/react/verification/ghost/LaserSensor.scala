package react.verification.ghost

//this is horizontal only
class LaserSensor(minAngle: Double,
                  maxAngle: Double,
                  samples: Int,
                  minRange: Double,
                  maxRange: Double,
                  resolution: Double,
                  topic: String,
                  rate: Double
                 ) extends Sensor(topic, rate) {

  //TODO intersection with Box2D ...

}
