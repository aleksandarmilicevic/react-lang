package react.examples

import react._
import react.robot._

import javax.swing._

class Remote(robot: Robot) extends JFrame {

  import java.awt._
  import java.awt.event._

  val label = new JLabel("use the left/right/up/down keys to direct the turtle")

  setTitle(robot.id + ", keyboard teleop")

  val log = Array("", "", "", "", "", "", "", "", "")
  val logTextField = new JTextArea("", log.length, 40)
  //val logTextField = new JTextField("", 40)
  var idx = 0
  def logText = {
    val buffer = new scala.collection.mutable.StringBuilder
    for (i <- idx + 1 until log.length) {
      buffer.append(log(i))
      buffer.append("\n")
    }
    for (i <- 0 until idx + 1) {
      buffer.append(log(i))
      buffer.append("\n")
    }
    buffer.toString
  }
  def addToLog(str: String) = {
    log(idx) = str
    idx = (idx + 1) % log.length
    logTextField.setText(logText)
  }

  addWindowListener(new WindowAdapter() {
    override def windowClosing(evt: WindowEvent) {
      System.exit(0)
    }
  })
    
  private def k2s(k: Key.Value) = k match {
    case Key.UP =>    "up"
    case Key.DOWN =>  "down"
    case Key.LEFT =>  "left"
    case Key.RIGHT => "right"
    case Key.NONE =>  ""
  }

  private class KeyPressed(k: Key.Value) extends AbstractAction {
    def actionPerformed(e: ActionEvent) = {
      addToLog("sending: " + k2s(k))
      robot send k
    }
  }

  private def setAction(key: String, action: AbstractAction) {
    //println("setting action for " + key)
    //val im = logTextField.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
    val im = logTextField.getInputMap()
    im.put(KeyStroke.getKeyStroke(key), key)
    val am = logTextField.getActionMap()
    am.put(key, action)
  }

  private def initGUI {
    setLayout(new BorderLayout())

    //val centerPane = new JPanel

    //centerPane.add(label)
    add(label, BorderLayout.NORTH)
    //centerPane.add(Box.createVerticalGlue())
    logTextField.setEditable(false)
    logTextField.setText(logText)
    //centerPane.add(logTextField)
    add(logTextField, BorderLayout.CENTER)

    //add(topRow, BorderLayout.NORTH)
    //add(centerPane, BorderLayout.CENTER)
    //add(status, BorderLayout.SOUTH)
    
    setAction("UP",    new KeyPressed(Key.UP   ))
    setAction("DOWN",  new KeyPressed(Key.DOWN ))
    setAction("LEFT",  new KeyPressed(Key.LEFT ))
    setAction("RIGHT", new KeyPressed(Key.RIGHT))

    //setSize(600, 500);
    pack();
  }

  initGUI
  setVisible(true)
}
