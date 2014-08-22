package react.verification

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


  //TODO ...
  // -the environement:
  //   -putting something in the world, e.g., some block (occupied cells) to avoid
  // -robots:
  //   -adding the robots
  //   -setting their initial state
  // -ghosts (to close the system)
  //   -simulate user-input
  //   -simulate the sensors
  // -global time
  // -options for the model checker:
  //   -backends
  //   -search strategies
  //   -bounding depth

  ///////////////////

  var time = 0
  
  type State = Array[Byte]
  
  protected var statefulObj: List[Stateful] = Nil
  
  protected def addStatefulObject[T](obj: T) = {
    val s =new Stateful {
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
  }

}
