package react.verification

import react._
import react.verification.environment._
import react.verification.model._
import react.verification.ghost._
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
    boxes = b :: boxes
  }

  /* add a robot in the world */
  final def robot[T <: Robot](r: T, model: TwistGroundRobot): Unit = macro WorldMacros.addRobot[T]

  /* ghost to 'close' the world (simulate user input, ...) */
  final def ghost[T <: Ghost](g: T): Unit = macro WorldMacros.addGhost[T]

  ///////////////////////////////////////////////////
  // data structures for the elements in the world //
  ///////////////////////////////////////////////////
  
  var robots: List[Robot] = Nil
  var models: List[TwistGroundRobot] = Nil
  var ghosts: List[Ghost] = Nil
  var boxes: List[Box2D] = Nil

  protected def enclosure = {
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

  def allBoxes = {
    val b = models.map(_.boundingBox) ++ boxes
    enclosure ::: b
  }

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
      buffer.append("      as physical model executing command from " + m.topic + "\n")
      buffer.append("      bounding boxe: " + m.boundingBox + "\n")
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
    buffer.append("}\n")
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
  
  
  ////////////////
  // sync & co. //
  ////////////////
  
  //TODO not complete unless very strong assumptions on the scheduler

  def grabAllLocks {
    for (r <- robots) {
      val acquired = r.lock.tryLock(1000, java.util.concurrent.TimeUnit.MILLISECONDS)
      if (!acquired) {
        sys.error("Robot " + r + "has been busy for more than 1000ms. infinite loop ?!?")
      }
    }
    for (r <- models) {
      val acquired = r.lock.tryLock(1000, java.util.concurrent.TimeUnit.MILLISECONDS)
      if (!acquired) {
        sys.error("Model " + r + "has been busy for more than 1000ms. infinite loop ?!?")
      }
    }
  }

  def releaseAllLock {
    for (r <- robots) {
      r.lock.unlock()
    }
    for (r <- models) {
      r.lock.unlock()
    }
  }

  def waitUntilStable {
    releaseAllLock
    Thread.`yield`()
    grabAllLocks
  }
  
  ////////////////////////////////
  // save/load the system state //
  ////////////////////////////////

  type State = Array[Byte]
  
  protected var statefulObj: List[Stateful] = Nil
  
  lazy val totalLength = statefulObj.foldLeft(0)(_ + _.length)

  //TODO round using discretisation
  def getCurrentState: State = {
    val buffer = ByteBuffer.allocate(totalLength) 
    for(s <- statefulObj) s.serialize(buffer)
    buffer.array
  }

  //TODO round using discretisation
  def restoreState(s: State) {
    val buffer = ByteBuffer.wrap(s) 
    for(s <- statefulObj) s.deserilize(buffer)
    for(m <- models) {
      m.restored //make the state consistent again
    }
    //trigger an updateWorld
    dispatchBoxes  
  }

}

class WorldMacros(val c: Context) {

  import c.universe._

  def addRobot[T <: Robot : c.WeakTypeTag](r: c.Expr[T], model: c.Expr[TwistGroundRobot]): c.Expr[Unit] = {
    val id1 = Ident(TermName(c.freshName("id")))
    val id2 = Ident(TermName(c.freshName("id")))
    val st1 = addStatefulObject(c.Expr[T](id1))
    val st2 = addStatefulObject(c.Expr[TwistGroundRobot](id2))
    val tree = q"""
      val $id1 = $r
      val $id2 = $model
      $st1
      $st2
      $id2.robotId = $id1.id
      robots = $id1 :: robots
      models = $id2 :: models
    """
    c.Expr[Unit](tree)
  }

  def addGhost[T <: Ghost : c.WeakTypeTag](g: c.Expr[T]): c.Expr[Unit] = {
    val id = Ident(TermName(c.freshName("id")))
    val st = addStatefulObject(c.Expr[T](id))
    val tree = q"""
      val $id = $g
      $st
      ghosts = $id :: ghosts
      """
    c.Expr[Unit](tree)
  }

  protected def addStatefulObject[T: c.WeakTypeTag](obj: c.Expr[T]): Tree = {
    val t = c.prefix
    q"""
    val s = new Stateful {
      import Stateful._
      val o = $obj
      def length: Int = o.length
      def round: Unit = o.round($t)
      def serialize(out: java.nio.ByteBuffer): Unit = o.serialize(out)
      def deserilize(in: java.nio.ByteBuffer): Unit = o.deserilize(in)
      def description: String = o.description
    }
    statefulObj = s :: statefulObj
    """
  }
  
}
