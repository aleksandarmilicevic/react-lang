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

  // -the safety properties:
  //   -some default ones: no collision
  //   -user defined: avoid some particular location, ...
  def safetyProperty: Boolean
  
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
  def ghost(g: Ghost) {
    addStatefulObject(g)
    ghosts = g :: ghosts
  }

  //TODO connect to the MC and the executor ..

  ///////////////////

  var robots: List[Robot] = Nil
  var models: List[TwistGroundRobot] = Nil
  var ghosts: List[Ghost] = Nil
  var boxes: List[Box2D] = Nil

  var time = 0

  def allBoxes = {
    models.map(_.boundingBox) ++ boxes
  }

  def dispatchBoxes {
    val bbs = allBoxes
    for ( (m,i) <- models.zipWithIndex ) {
      val boxes = bbs.take(i) ::: bbs.drop(i+1)
      m.updateWorld(boxes)
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
  
  val totalLength = statefulObj.foldLeft(4)(_ + _.length)

  def getCurrentState: State = {
    val buffer = ByteBuffer.allocate(totalLength) 
    buffer.putInt(time)
    for(s <- statefulObj) s.serialize(buffer)
    buffer.array
  }

  def restoreState(s: State) {
    val buffer = ByteBuffer.wrap(s) 
    time = buffer.getInt
    for(s <- statefulObj) s.deserilize(buffer)
    for(m <- models) {
      m.restored //make the state consistent again
    }
    //trigger an updateWorld
    dispatchBoxes  
  }

}
