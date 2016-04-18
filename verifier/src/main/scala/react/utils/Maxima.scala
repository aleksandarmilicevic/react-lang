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
    val solver = java.lang.Runtime.getRuntime.exec(Array(command, "-q", "--disable-readline"), null, null)
    val output = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream()))
    val input = new BufferedReader(new InputStreamReader(solver.getInputStream()))
    val error = new BufferedReader(new InputStreamReader(solver.getErrorStream()))

    def readUntilNewInput: String = {
      val buffer = new StringBuilder
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
          if (line.trim matches "\\(%i\\d+\\)") {
            done = true
          } else {
            buffer.append(line)
          }
        }
      }
      buffer.toString
    }

    def write(txt: String) {
      Logger("Maxima -> ", Info, txt)
      output.write(txt)
      output.newLine()
      output.flush()
    }

    try {

      write("display2d:false$")
      readUntilNewInput
      for (i <- includes) {
        write(includeCommand(i))
        readUntilNewInput
      }

      def endOfCommand(str: String) = str.endsWith(";") || str.endsWith("$")
        
      val maximaOut = new StringBuilder

      //process the commands
      val lines = query.split("[\\r\\n]+").map(_.trim)
      assert(endOfCommand(lines.last))
      for (line <- lines) {
        write(line)
        if (endOfCommand(line)) {
          maximaOut.append(readUntilNewInput)
        }
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

  }
}
