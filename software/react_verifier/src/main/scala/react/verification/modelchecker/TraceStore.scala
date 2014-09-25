package react.verification.modelchecker

import react.utils._
import java.nio.file._
import java.nio.file.StandardOpenOption._
import java.nio.charset.Charset
import java.io._
import org.apache.commons.codec.binary.Hex
import ModelChecker._
import HashStateStore._

class TraceStore {

  protected var path: Path = null
  protected var writer: BufferedWriter = null
  val utf8 = Charset.forName("UTF-8")

  def openWriter {
    if (writer == null) {
      writer = Files.newBufferedWriter(path, utf8, WRITE, APPEND)
    }
  }

  def closeWriter {
    if (writer != null) {
      writer.close()
      writer = null
    }
  }

  def init {
    try {
      path = Files.createTempFile("react_trace_store", ".tmp")
      assert(Files.isWritable(path))
    } catch {
      case e: Exception =>
        Logger("TraceStore", LogWarning, "could not create temporary file (" + e + "), not storing the trace")
    }
  }

  init

  def clean {
    closeWriter
    if (path != null) {
      Files.deleteIfExists(path)
      path = null
    }
  }

  override def finalize {
    clean
  }

  def add(t: Trace) {
    openWriter
    
    def printLabel(l: Label) {
      def clean(s: String) = s.replaceAll("\n", " ").replaceAll("\r", " ")
      writer.write(l.length.toString)
      writer.newLine
      for (s <- l) {
        writer.write(clean(s))
        writer.newLine()
      }
    }

    val last = new RichState(t.stop)
    val head = new RichState(t.start)
    writer.write(last.toString)
    writer.write(" ")
    writer.write(head.toString)
    writer.write(" " + t.transitions.size)
    writer.newLine()
    for ( (l,s) <- t.transitions ) {
      printLabel(l)
      writer.write((new RichState(s)).toString)
      writer.newLine()
    }
  }

  protected def parseTrace = {
    closeWriter //to flush anything left
    var reader = Files.newBufferedReader(path, utf8)

    def mkState(str: String) = Hex.decodeHex(str.toCharArray)
    val predMap = collection.mutable.HashMap[RichState, Trace]()

    def parseLabel: Label = {
      val n = reader.readLine.toInt
      val raw = for(_ <- 0 until n) yield {
        reader.readLine
      }
      raw.toList
    }
    
    while(reader.ready) {
      val line = reader.readLine.split(" ")
      assert(line.size == 3)
      val k = mkState(line(0))
      val s = mkState(line(1))
      val n = line(2).toInt
      val tail = for(_ <- 0 until n) yield {
        val descr = parseLabel
        val state = mkState(reader.readLine)
        (descr -> state)
      }
      val t = new Trace(s, tail.toList)
      predMap(k) = t
    }

    predMap
  }

  def makeTrace(t: Trace): Trace = {
    val predMap = parseTrace
    def mk(tr: Trace): Trace = {
      if (predMap contains tr.start) {
        val prefix = predMap(tr.start)
        mk(prefix concat tr)
      } else {
        tr
      }
    }
    mk(t)
  }

  def states: Iterable[State] = {
    parseTrace.values.flatMap(_.states)
  }

}
