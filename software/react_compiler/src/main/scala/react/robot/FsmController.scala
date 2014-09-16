package react.robot

import react._
import react.message._
import react.runtime._

import scala.language.experimental.macros
import react.rewriting.{RobotMacros, ExplorableMacros}


trait FsmController extends Robot {

  //////////////////
  // for the user //
  //////////////////

  def state(sid: Symbol)(body: Unit): Unit = macro RobotMacros.makeState
    
  def nextState(sym: Symbol) = {
    assert(currentState != null, "current state is null")
    getCurrentState.deregister(exec)
    currentState = sym
    getCurrentState.register(exec)
  }

  def initialState(sym: Symbol) = {
    assert(currentState == null, "cannot have more than one initialState")
    currentState = sym
  }

  /////////////////////
  // for the runtime //
  /////////////////////

  protected abstract class State(stateId: Symbol) extends Controller {
    val id = FsmController.this.id
    val lock = FsmController.this.lock
  }

  protected val statesMap = scala.collection.mutable.HashMap[Symbol,State]()
  var currentState: Symbol = null
  private def getCurrentState: State = {
    assert(statesMap contains currentState, "state " + currentState + " does not exists")
    statesMap(currentState)
  }

  override def getAllTasks = {
    super.getAllTasks ++ statesMap.values.flatMap(_.getAllTasks)
  }

  override def send(any: Any) {
    getCurrentState.send(any)
    super.send(any)
  }

  override def register(exec: Executor) {
    super.register(exec)
    assert(currentState != null, "initial state is not specified")
    getCurrentState.register(exec)
  }

  override def deregister(exec: Executor) {
    getCurrentState.deregister(exec)
    super.deregister(exec)
  }
  
  ////////////////////
  // fixing symbols //
  ////////////////////
  // this is needed due to the way the compiler currently handles symbols and prevent capture of external symbols
  
  private var _tasks: List[ScheduledTask] = Nil
  private var _handlers: List[PartialFunction[Any, Unit]] = Nil
  private var _sensors: List[MessageListenerWrapper] = Nil
  protected def snapshotData {
    _tasks    = tasks
    _handlers = handlers
    _sensors  = sensors
    tasks    = Nil
    handlers = Nil
    sensors  = Nil
  }
  protected def fixData(s: State) {
    for (t <- tasks.reverse) s.addTask(t)
    for (t <- handlers.reverse) s.addHandler(t)
    for (t <- sensors.reverse) s.addSensor(t)
    tasks    = _tasks
    handlers = _handlers
    sensors  = _sensors
  }


}
