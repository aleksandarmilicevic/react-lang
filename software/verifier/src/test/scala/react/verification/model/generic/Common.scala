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

  val model1 = """
(bbox -0.1 -0.1 0.2 0.2 0)
(input motorLeft  16)
(input motorRight 23)
(let angularSpeed (v) (* (/ v 100) Pi))
(let dl () (* wheelRadius (* (angularSpeed motorLeft) t)))
(let dr () (* wheelRadius (* (angularSpeed motorRight) t)))
(let centerOfRotation () (/ (* (+ dr dl) wheelSpacing) (* 2 (- dr dl))))
(let angle () (/ (- dr dl) wheelSpacing))
(let dx () (* centerOfRotation (sin angle)))
(let dy () (* centerOfRotation (- 1 (cos angle))))
(output x (+ x (* dx (cos yaw) (* -1 (* dy (sin yaw))))))
(output y (+ y (* dx (sin yaw) (* dy (cos yaw)))))
(output yaw (+ yaw angle)) 
"""

  val model1NoLet = """
(bbox -0.1 -0.1 0.2 0.2 0)
(input motorLeft  16)
(input motorRight 23)
(assert (= dl (* wheelRadius (* (* (/ motorLeft 100) Pi) t))))
(assert (= dr (* wheelRadius (* (* (/ motorRight 100) Pi) t))))
(assert (= centerOfRotation (/ (* (+ dr dl) wheelSpacing) (* 2 (- dr dl)))))
(assert (= angle (/ (- dr dl) wheelSpacing)))
(assert (= dx (* centerOfRotation (sin angle))))
(assert (= dy (* centerOfRotation (- 1 (cos angle)))))
(output x (+ x (* (* dx (cos yaw)) (* -1 (* dy (sin yaw))))))
(assert (= y (+ y (* (* dx (sin yaw)) (* dy (cos yaw))))))
(assert (= yaw (+ yaw angle)))
"""

}
