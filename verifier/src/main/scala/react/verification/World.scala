package react.verification

import react.Robot
import react.verification.environment._
import react.verification.model._
import react.verification.ghost._
import react.verification.modelchecker.ProductBranchingPoint
import java.nio.ByteBuffer

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context


//For the moment a simple 2D world
abstract class World extends Playground {

  //////////////////
  // for the user //
  //////////////////

  // safety properties:
  //   -some default ones: no collision
  //   -user defined: avoid some particular location, ...
  def safe: Boolean
  
  /* add an obstacle in the world */
  final def obstacle(b: Box2D) {
    boxes ::= b
  }

  /* add a robot in the world */
  final def robot[T1 <: Robot, T2 <: GroundRobot](r: T1, m: T2): Unit = macro WorldMacros.addRobot[T1,T2]

  /* ghost to 'close' the world (simulate user input, ...) */
  final def ghost[T <: Ghost](g: T): Unit = macro WorldMacros.addGhost[T]
  
  /* an object with a state that needs to be saved/restored */
  final def stateful[T](s: T): Unit = macro WorldMacros.addStateful[T]

  /* add an reachability goal for the BMC */
  final def goal(i: Int, b: Box2D) {
    targets ::= (i -> b)
  }

  ///////////////////////////////////////////////////
  // data structures for the elements in the world //
  ///////////////////////////////////////////////////
  
  var robots: List[Robot] = Nil
  var models: List[GroundRobot] = Nil
  var ghosts: List[Ghost] = Nil
  var boxes: List[Box2D] = Nil
  var targets: List[(Int,Box2D)] = Nil

  def enclosure = {
    val wall = 0.1
    if (enclosed) {
      List(
        new Box2D(xMin - wall, yMin, 0, wall, yMax - yMin),
        new Box2D(xMin, yMin - wall, 0, xMax - xMin, wall),
        new Box2D(xMax, yMin, 0, wall, yMax - yMin),
        new Box2D(xMin, yMax, 0, xMax - xMin, wall)
      )
    }
    else Nil
  }

  lazy val envBoxes = boxes ::: enclosure
  def modelBoxes = models.map(_.boundingBox)
  def allBoxes = modelBoxes ::: envBoxes //returns the model BB first!

  def dispatchBoxes {
    val bbs = allBoxes
    for ( (m,i) <- models.zipWithIndex ) {
      val boxes = bbs.take(i) ::: bbs.drop(i+1)
      m.updateWorld(boxes)
    }
  }

  def elapse(dt: Int) {
    for (m <- models) {
      m.elapse(dt)
    }
    dispatchBoxes
  }

  def elapseBP(dt: Int) = {
    new ProductBranchingPoint(models.map(_.elapseBP(dt)), () => dispatchBoxes)
  }

  override def toString = {
    val buffer = new StringBuilder(1024)
    buffer.append("world {\n")
    buffer.append("  x ∈ ["+xMin+", "+xMax+"], steps of " + xDiscretization + "\n")
    buffer.append("  y ∈ ["+yMin+", "+yMax+"], steps of " + yDiscretization + "\n")
    buffer.append("  robots:\n")
    for (r <- robots) {
      buffer.append("    " + r.id + ": " + r +"\n" )
    }
    buffer.append("  models:\n")
    for (m <- models) {
      buffer.append("    " + m + "\n")
      buffer.append("      bounding box: " + m.boundingBox + "\n")
    }
    buffer.append("  ghosts:\n")
    for (g <- ghosts) {
      buffer.append("    " + g + "\n")
    }
    buffer.append("  obstacles:\n")
    for (b <- enclosure) {
      buffer.append("    " + b + " (enclosure)\n")
    }
    for (b <- boxes) {
      buffer.append("    " + b + "\n")
    }
    buffer.toString
  }

  def currentState = {
    val buffer = new StringBuilder(1024)
    buffer.append("state saved by MC:\n")
    for (s <- statefulObj) {
      buffer.append("  " + s.longDescription + "\n" )
    }
    buffer.toString
  }

  def stateSpaceDescription = {
    val buffer = new StringBuilder(1024)
    buffer.append("state space has " + totalLength + " bytes (without scheduler).\n")
    for (s <- statefulObj){
      buffer.append("  ")
      buffer.append(s.description)
      buffer.append("\n")
    }
    buffer.toString
  }

  def round {
    for (s <- statefulObj) s.round
  }

  def concretizations = {
    //get the stateful objects and enumerate the concretizations
    def mkIter(it1: StateModifierIterator, s: Stateful): StateModifierIterator = {
      new StateModifierIterator {
        val it2 = s.concretize
        var first = true
        def hasNext = { it1.hasNext || it2.hasNext }
        def next {
          if (first) {
            assert(!it2.isEmpty)
            it2.next()
            if (it1.hasNext) {
              it1.next
            }
            first = false
          } else {
            if (it1.hasNext) {
              it1.next
            } else if (it2.hasNext) {
              it2.next
              it1.reset
              if (it1.hasNext) {
                it1.next
              }
            } else {
              assert(false, "hasNext == false")
            }
          }
        }
        def reset {
          first = true
          it1.reset
          it2.reset
        }
      }
    }
    val empty = new StateModifierIterator{
      def hasNext = false
      def next { }
      def reset { }
    }
    statefulObj.foldLeft(empty)(mkIter)
  }

  def inBounds = {
    models.forall( m => m.x >= xMin && m.x <= xMax && m.y >= yMin && m.y <= yMax )
  }
  
  
  ////////////////////////////////
  // save/load the system state //
  ////////////////////////////////

  type State = Array[Byte]
  
  protected var statefulObj: List[Stateful] = Nil
  
  lazy val totalLength = statefulObj.foldLeft(0)(_ + _.length)

  def getCurrentState: State = {
    val buffer = ByteBuffer.allocate(totalLength) 
    for(s <- statefulObj) s.serialize(buffer)
    buffer.array
  }

  def restoreState(s: State) {
    val buffer = ByteBuffer.wrap(s) 
    for(s <- statefulObj) s.deserilize(buffer)
    for(r <- robots) {
      r.restored //make the state consistent again
    }
    for(m <- models) {
      m.restored //make the state consistent again
    }
    //trigger an updateWorld
    dispatchBoxes  
  }

  def worldAgnostic: Boolean = statefulObj.forall(_.worldAgnostic)

}

class WorldMacros(val c: Context) {

  import c.universe._

  def addRobot[T1 <: Robot : c.WeakTypeTag, T2 <: GroundRobot : c.WeakTypeTag](r: c.Expr[T1], m: c.Expr[T2]): c.Expr[Unit] = {
    val id1 = Ident(TermName(c.freshName("id")))
    val id2 = Ident(TermName(c.freshName("id")))
    val t = c.prefix
    val tree = q"""
      val $id1 = $r
      val $id2 = $m
      statefulObj = Stateful.makeStateful($id1, $t) :: statefulObj
      statefulObj = Stateful.makeStateful($id2, $t) :: statefulObj
      $id2.robotId = $id1.id
      robots = $id1 :: robots
      models = $id2 :: models
    """
    c.Expr[Unit](tree)
  }

  def addGhost[T <: Ghost : c.WeakTypeTag](g: c.Expr[T]): c.Expr[Unit] = {
    val id = Ident(TermName(c.freshName("id")))
    val t = c.prefix
    val tree = q"""
      val $id = $g
      statefulObj = Stateful.makeStateful($id, $t) :: statefulObj
      ghosts = $id :: ghosts
      """
    c.Expr[Unit](tree)
  }

  def addStateful[T: c.WeakTypeTag](s: c.Expr[T]): c.Expr[Unit] = {
    val id = Ident(TermName(c.freshName("id")))
    val t = c.prefix
    val tree = q"""
      val $id = $s
      statefulObj = Stateful.makeStateful($id, $t) :: statefulObj
      """
    c.Expr[Unit](tree)
  }

}
