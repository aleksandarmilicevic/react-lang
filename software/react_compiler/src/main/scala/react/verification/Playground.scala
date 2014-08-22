package react.verification

/** defines the boundaries of where the robots will evolve during verification */
trait Playground {

  val xMin: Int
  val xMax: Int
  val xDiscretization: Double

  val yMin: Int
  val yMax: Int
  val yDiscretization: Double

//val zMin: Int
//val zMax: Int
//val zDiscretization: Double

}
