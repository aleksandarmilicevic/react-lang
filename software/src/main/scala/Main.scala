package react

import react.utils.IO

object Main {

  def main(args: Array[String]) {
    Config(args)
    Config.input match {
      case fn :: _ => process(fn, IO.readTextFile(fn))
      case Nil => process("stdin", IO.readStdin)
    }
  }

  def process(fileName: String, content: String) {
    //TODO
    println("Hello world!")
  }

}
