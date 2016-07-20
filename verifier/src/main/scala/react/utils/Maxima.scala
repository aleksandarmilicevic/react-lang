package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Maxima {

  val command = "maxima"

  lazy val isPresent =
    try SysCmd(Array(command, "-h"))._1 == 0 
    catch { case _: Throwable => false }
  
  val timeout = 10 * 1000 // 10s Timeout

  val includePrefix = "verifier/src/main/maxima/"

  val includes = List(
    "utils.mac",
    "dRealPrinter.mac",
    "quaternions.mac",
    "frame.mac",
    "joint.mac",
    "modelPrinter.mac"
  )

  def includeCommand(file: String) = "load(\""+includePrefix+file+"\") $"

  def apply(query: String) = {

    import java.io._
    val solver = java.lang.Runtime.getRuntime.exec(Array(command, "--very-quiet", "--disable-readline"), null, null)
    val output = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream()))
    val input = new BufferedReader(new InputStreamReader(solver.getInputStream()))
    val error = new BufferedReader(new InputStreamReader(solver.getErrorStream()))

    def readUntilNewInput: String = {
      val buffer = new StringBuilder
      buffer.toString
    }

    def write(txt: String) {
      Logger("Maxima -> ", Info, txt)
      output.write(txt)
      output.newLine()
      output.flush()
    }

    def endOfCommand(str: String) = str.endsWith(";") || str.endsWith("$")
    def isSilent(str: String) = str.endsWith("$")

    val maximaOut = new StringBuilder

    def processLine(str: String) = {
      write(str);
      if (endOfCommand(str) && !isSilent(str)) {
        val to = System.currentTimeMillis + timeout
        var done = false
        while (!done) {
          while (!input.ready && System.currentTimeMillis <= to) {
            Thread.sleep(10)
          }
          if (System.currentTimeMillis > to) {
            sys.error("Timeout")
          } else {
            val line = input.readLine
            Logger("Maxima <- ", Info, line)
            if (line.trim != "") {
              maximaOut.append(line)
              maximaOut.append("\n")
              done = true
            }
          }
        }
      }
    }

    try {

      processLine("display2d:false$")
      for (i <- includes) {
        processLine(includeCommand(i))
      }

      //process the commands
      val lines = query.split("[\\r\\n]+").map(_.trim)
      assert(endOfCommand(lines.last))

      for (line <- lines) {
        processLine(line)
      }
      
      //quit
      write("quit();")

    } catch {
      case t: Throwable =>
        solver.destroy
        throw t
    } finally {
      solver.waitFor
      output.close
      input.close
      error.close
    }
    maximaOut.toString
  }
}
