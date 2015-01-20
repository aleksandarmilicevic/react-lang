package react.verification.model.generic

object Resources {

  val playground = new react.verification.Playground {
    val xMin = -1
    val xMax = 1
    val xDiscretization = 0.015625

    val yMin = -1
    val yMax = 1
    val yDiscretization = 0.015625

    val enclosed = true
                      
    val fpDiscretization = 0.015625

  }

  val path = "verifier/src/test/resources/"

}
