package react.verification.ghost

trait Ghost {

  //when can this ghost do something ?
  //-1 ⇔ never
  def nextActionIn = -1

  //period of the events
  def period = 1

}
