package react.verification.modelchecker

/* an interface for the places where the system might branch */
trait BranchingPoint {

  /* the number of possible action from this component */
  def alternatives: Int

  /* execute the given alternative, alt ∈ [0,alternatives) */
  def act(alt: Int): List[String]

}


/** a branching point for a list of branching points */
class SumBranchingPoint(points: List[BranchingPoint]) extends BranchingPoint {

  val alts = points.foldLeft(0)(_ + _.alternatives)
  def alternatives = alts

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


/** a branching point that is the cartesian product for a list of branching points */
class ProductBranchingPoint(points: List[BranchingPoint], post: () => Unit) extends BranchingPoint {

  val possibilities = points.map(_.alternatives)

  def alternatives = possibilities.product

  def act(alt: Int) = {
    var labels = List[String]()
    points.foldLeft(alt)( (acc, p) => {
      val idx = acc % p.alternatives
      val rem = acc / p.alternatives
      labels = p.act(idx) ::: labels
      rem
    })
    post()
    labels.reverse
  }

}
