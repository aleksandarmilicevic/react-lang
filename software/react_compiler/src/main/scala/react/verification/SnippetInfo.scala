package react.verification

//TODO keep track of what a method/handler does ...

trait SnippetInfo {

  /** an unique ID for the current snippet */
  val snippetID = SnippetInfo.freshID

  /** the ID of the robot containing this snippet */
  val robotID: Option[String]

  //TODO function ID ?

  /** the variables read by this snippet */
  val readVar: Option[List[String]]

  /** the variables written by this snippet */
  val writeVar: Option[List[String]]

  /** send messages to the following ROS topics */
  val publishTo: Option[List[(String,String)]]

  /** receive messages from the following ROS topics */
  val subscribeTo: Option[List[(String,String)]]

  /** call the (REACT) send on the following robots */
  val sendTo: Option[List[String]]

  /** method calls */
  val call: Option[List[String]] //TODO more precise: internal vs external call ?

}

object SnippetInfo {

  protected val cnt = new java.util.concurrent.atomic.AtomicInteger

  def freshID = cnt.getAndIncrement

  //TODO also lookup table for snippets ?

}
