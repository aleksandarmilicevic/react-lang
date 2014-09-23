package react.verification

import ModelChecker._

//Warning: the iterator skips the first location
class Trace(val start: State, val transitions: List[(Label,State)]) extends Iterable[(Label,State)] {

  /** Returns the number of transition in the trace (#state -1) */
  def length = transitions.length

  def states = start :: transitions.map(_._2)

  def labels = transitions.map(_._1)

  def stop = if (transitions.length == 0) start else transitions.last._2

  def compact = new Trace(start, List(labels.flatten -> stop))

  def step = {
    if (transitions.isEmpty) {
      throw new java.util.NoSuchElementException("step called on an empty trace")
    }
    val (t,b) = transitions.head
    ((start, t), new Trace(b, transitions.tail))
  }

  override def iterator = transitions.iterator

  private def mkTriple(acc: List[(State,Label,State)], current: State, rest: List[(Label,State)]): List[(State,Label,State)] = rest match {
    case (b,a)::xs => mkTriple( (current,b,a)::acc, a, xs)
    case Nil => acc.reverse
  }

  def extremities: (State,State) = (start, stop)

  def innerStates: List[State] = transitions.map(_._2).dropRight(1)

  def triples: List[(State,Label,State)] = mkTriple(Nil, start, transitions)

  def append(t: Label, s: State) = new Trace(start, transitions ::: List((t,s)))

  def concat(t: Label, trc: Trace) = new Trace(start, transitions ::: List(t -> trc.start) ::: trc.transitions)
  def concat(trc: Trace) = {
    assert(trc.start == stop)
    new Trace(start, transitions ::: trc.transitions)
  }

  def prepend(s: State, t: Label) = new Trace(s, (t,start) :: transitions )

  def reverse = new Trace(stop, triples.reverse map ( t => (t._2, t._1)))

  def split(at: State => Boolean): List[Trace] = {
    var acc: List[Trace] = Nil
    var current = this
    while (!current.transitions.isEmpty) {
      val t1Prefix = current.transitions.takeWhile(elt => !at(elt._2))
      val t2Augmented = current.transitions.drop(t1Prefix.length)
      if(t2Augmented.isEmpty) {
        acc = current :: acc
        current = new Trace(current.stop, Nil)
      } else {
        acc = new Trace(current.start, t1Prefix :+ t2Augmented.head) :: acc
        current = new Trace(t2Augmented.head._2, t2Augmented.tail)
      }
    }
    assert(current.transitions.isEmpty)
    acc.reverse
  }

  def split(at: State): List[Trace] = split(_ == at)

  def splitAfter(n: Int): (Trace,Trace) = {
    val (t1, t2) = transitions.splitAt(n)
    val part1 = new Trace(start, t1)
    val part2 = new Trace(part1.stop, t2)
    (part1, part2)
  }

  override def toString = start + transitions.map( p => (p._1 + " => "+p._2)).mkString(" => ", " => ", "")

  override def equals(any: Any) = any match {
    case t: Trace => start == t.start && transitions == t.transitions
    case _ => false
  }

}

object Trace {
  def apply(start: State, transitions: (Label,State)*) = new Trace(start, transitions.toList)
}
