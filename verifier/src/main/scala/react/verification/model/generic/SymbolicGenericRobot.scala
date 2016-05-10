package react.verification.model.generic

import react.message._
import react.runtime.MessageListenerRW
import react.Executor
import react.robot._
import react.utils._
import react.verification.Playground
import react.verification.model._
import react.verification.environment._
import react.verification.modelchecker.BranchingPoint
import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import Utils._

class SymbolicGenericRobot( _id: String,
                            _pg: Playground,
                            _bBox: Box2D,
                            _frame: Frame,
                            _inputs: List[Input],
                            _dynamic: List[Variable],
                            _transient: Map[Variable,Double],
                            _constraints: Formula ) extends GenericRobot(_id, _pg, _bBox, _frame, _inputs, _dynamic, _transient, _constraints) {

  var symStore = Map[Variable, Variable]()
  
  override def register(exec: Executor) {
    //super.register(exec)

    for(i <- inputs) {
      val listener = new MessageListenerRW[std_msgs.String]{
        def robotID = id
        override def read = Some(Set())
        override def written = Some(Set(i.v.toString))
        val name = i.topic
        def onNewMessage(message: std_msgs.String) {
          lock.lock
          try {
            val name = message.getData
            //println( i.v + " -> " + name)
            symStore += i.v -> Variable(name).setType(Real)
          } finally lock.unlock
          exec.messageDelivered
        }
      }
      val sub = exec.getSubscriber[std_msgs.String](id + "/" + i.topic, std_msgs.String._TYPE)
      sub.addMessageListener(listener)
    }

  }

  override protected def moveFor(t: Int) = {
    //nothing
  }
  
  override def deregister(exec: Executor) {
    super.deregister(exec)
    //TODO deregister the inputs
  }

  override protected def getKnown: Map[Variable,Formula] = {
    inputs.flatMap( i => symStore.get(i.v).map( v => i.v -> v ) ).toMap
  }

  protected def getKnownBounds = {
    for (i <- inputs;
         v <- symStore.get(i.v)) yield {
      And( Lt(v, Literal( 100.0)),
           Gt(v, Literal(-100.0)) )
    }
  }
  
  override def stateEquations(index: Int): Formula = {
    val f1 = super.stateEquations(index)
    And(FormulaUtils.getConjuncts(f1) ++ getKnownBounds :_*)
  }

  override def unrollEquations(fromIndex: Int, dt: Int): Formula = {
    val f1 = super.unrollEquations(fromIndex, dt)
    //TODO this is a temporary hack (when we have DAE, it will be gone)
    val vf = variablesAt(fromIndex)
    val toIndex = fromIndex + 1
    val vt = variablesAt(toIndex)
    val maxChange = 22.5
    val f2 = And(inputs.map( i => {
      val i1 = vf(i.v)
      val i2 = vt(i.v)
      And(
        Leq(Minus(i1, i2), Literal( maxChange)),
        Geq(Minus(i1, i2), Literal(-maxChange))
      )
    }):_*)
    //And(f1, f2)
    And(f1)
  }

}

object SymbolicGenericRobot {

  def apply(id: String, pg: Playground, fileName: String): SymbolicGenericRobot = {
    val gr = GenericRobot(id, pg, fileName)
    new SymbolicGenericRobot(gr.id, gr.pg, gr.bBox, gr.frame, gr.inputs, gr.dynamic, gr.transient, gr.constraints)
  }

}
