package react.verification.model

import react.robot.Angle

trait Motion {
  protected def moveFor(t: Int): Unit
}

trait Motion2D extends Motion {
  var x = 0.0
  var y = 0.0
  var _orientation = 0.0

  def orientation = _orientation
  def orientation_=(d: Double) {
    _orientation = Angle.normalize(d)
  }
}
