package react.verification.modelchecker

/* an interface for the places where the system might branch */
trait BranchingPoint {

  /* the number of possible action from this component */
  def alternatives: Int

  /* execute the given alternative, alt ∈ [0,alternatives) */
  def act(alt: Int): List[String]

}


/** a branching point for a list of branching points */
class BranchingPoints(points: List[BranchingPoint]) extends BranchingPoint {

  def alternatives = points.foldLeft(0)(_ + _.alternatives)

  def act(alt: Int) = {
    def traverse(alt: Int, points: List[BranchingPoint]): List[String] = {
      val a = points.head.alternatives
      if (alt >= a) {
        traverse(alt - a, points.tail)
      } else {
        points.head.act(alt)
      }
    }
    traverse(alt, points)
  }

}
