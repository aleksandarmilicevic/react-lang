package react.verification

import react._
import react.verification.environment._
import react.verification.model._
import react.verification.ghost._
import java.nio.ByteBuffer

//for verification purpose, we need to provide a model of the environement (dimensions, etc.)

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
  def obstacle(b: Box2D) {
    boxes = b :: boxes
  }

  /* add a robot in the world */
  def robot[M <: Robot](r: M, model: TwistGroundRobot) {
    addStatefulObject(r)
    addStatefulObject(model)
    model.robotId = r.id
    robots = r :: robots
    models = model :: models
  }

  /* ghost to 'close' the world (simulate user input, ...) */
  def ghost[M <: Ghost](g: M) {
    addStatefulObject(g)
    ghosts = g :: ghosts
  }

  ///////////////////////////////////////////////////
  // data structures for the elements in the world //
  ///////////////////////////////////////////////////
  
  var robots: List[Robot] = Nil
  var models: List[TwistGroundRobot] = Nil
  var ghosts: List[Ghost] = Nil
  var boxes: List[Box2D] = Nil

  def allBoxes = {
    val b = models.map(_.boundingBox) ++ boxes
    if (enclosed) {
       new Box2D(xMin, yMin, 0, xMax - xMin, yMax - yMin) :: b
    } else {
       b
    }
  }

  def dispatchBoxes {
    val bbs = allBoxes
    for ( (m,i) <- models.zipWithIndex ) {
      val boxes = bbs.take(i) ::: bbs.drop(i+1)
      m.updateWorld(boxes)
    }
  }
  
  
  ////////////////
  // time & co. //
  ////////////////
  
  //TODO not complete
  //better alternative might be to grab all locks, there release and grab again, if the locks are fair that should work
  def waitUntilStable {
    for (r <- robots) {
      val acquired = r.lock.tryLock(1000, java.util.concurrent.TimeUnit.MILLISECONDS)
      if (!acquired) {
        sys.error("Robot " + r + "has been busy for more than 1000ms. infinite loop ?!?")
      } else {
        r.lock.unlock
      }
    }
  }
  
  ////////////////////////////////
  // save/load the system state //
  ////////////////////////////////

  type State = Array[Byte]
  
  protected var statefulObj: List[Stateful] = Nil
  
  protected def addStatefulObject[T](obj: T) = {
    val s = new Stateful {
      import Stateful._
      val o = obj
      def length: Int = o.length(World.this)
      def serialize(out: ByteBuffer): Unit = o.serialize(World.this, out)
      def deserilize(in: ByteBuffer): Unit = o.deserilize(World.this, in)
    }
    statefulObj = s :: statefulObj
  }
  
  val totalLength = statefulObj.foldLeft(0)(_ + _.length)

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
