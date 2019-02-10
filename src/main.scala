import scala.swing.SimpleSwingApplication

import scala.swing.Swing._
import scala.swing.Frame
import scala.collection.mutable.Buffer
import scala.swing.Component
import scala.swing.event.KeyPressed
import scala.swing.event._
import java.awt.Graphics2D
import java.util.{ Timer, TimerTask }
import java.awt.BasicStroke
import java.awt.Color._
import scala.collection.mutable.Queue
import java.awt.Font
import java.awt.image.BufferedImage
import javax.swing.WindowConstants.EXIT_ON_CLOSE
object GAMED {
  val w = 800
  val h = 600
  var rspeed = 0.1
  var speed = 10
}
object main extends SimpleSwingApplication {

  val area = new Component {
    var latest = new BufferedImage(GAMED.w, GAMED.h, BufferedImage.TYPE_INT_ARGB)
    override def paintComponent(gd: Graphics2D) = {
      gd.drawImage(latest, 0, 0, null)
    }
    val task = new TimerTask() {
      def run() = {
        GameInst.processInput()
        latest = GameInst.thisFrame()
        repaint()
      }
    }
    new Timer().schedule(task, 0, 50)
    reactions += {
      case keys: KeyPressed  => GameInst.keyD(keys.key)
      case keys: KeyReleased => GameInst.keyU(keys.key)
    }
    focusable = true
    requestFocus
    listenTo(this.keys)
  }

  val top = new Frame() {
    peer.setDefaultCloseOperation(EXIT_ON_CLOSE)
    preferredSize = (GAMED.w, GAMED.h)
    contents = area
    pack
  }
}

