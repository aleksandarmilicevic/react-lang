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

  /** should we consider the boundaries as walls (for the sensors), or just discard anything beyond */
  val enclosed: Boolean

  /** float/double rounding (var different from x/y) */
  val fpDiscretization: Double

}
