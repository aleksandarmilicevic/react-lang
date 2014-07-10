package react

import utils._

/** A default configuration class */
class Config {
  
  private var options = List[Arg.Def]()

  def newOption(opt: Arg.Key, fct: Arg.Spec, doc: Arg.Doc) {
    options = (opt, fct, doc) :: options
  }
  
  var input: List[String] = Nil
  /** process arguments that do not belong to an option (i.e. the input files). */
  def default(arg: String) {
    input = arg :: input
  }
  
  val usage = "..."

  def apply(args: Seq[String]) {
    Arg.process(options, default, usage)(args)
  }

}

/** default configuration object */
object Config extends Config {
  
  //verbosity
  newOption("-v", Arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")
  newOption("--hide", Arg.String( str => Logger.disallow(str)), "hide the output with given prefix.")
  newOption("--noAssert", Arg.Unit(() => Logger.disableAssert), "remove some assertions.")

  //general reporting option
  var report = false
  var reportOutput: Option[String] = None

  newOption("-r", Arg.Unit(() => report = true), "output a report (with a default name).")
  newOption("--report", Arg.String(str => { report = true; reportOutput = Some(str) } ), "output a report with given name.")

  //general config stuff
  newOption("--maxChildren", Arg.Int ( i => utils.SysCmd.maxChildren = i), "limit the number of children that can be spawned at the same time (default: no limit).")

  var solverCmd = Array("z3", "-smt2", "-in")
  newOption("--smtSolver", Arg.String(str => solverCmd = str.split(" ")), "The smt sovler (+ options) to use (default: \"z3 -smt2 -in\").")

}
