package react.verification

import react.utils._

class McRunner(opts: McOptions, newWorld: () => World) {


  protected def mkProxy = new WorldProxy( newWorld() )

  var mc: ModelChecker = null

  def run: Nothing = {
    val pr = mkProxy
    mc = new ModelChecker(pr, opts)
    mc.init
    while(mc.oneStep) {}
    pr.shutdown
    mc.printStats
    mc = null
    Runtime.getRuntime().halt(0)
    sys.error("halt did not work")
  }

  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        Console.err.println("shutdown hook")
        val m = mc
        if (m != null)
          m.printStats
        Runtime.getRuntime().halt(0)
        sys.error("halt did not work")
      }
    }
  )


}
