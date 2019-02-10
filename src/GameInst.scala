import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.RenderingHints
import scala.swing.event.Key
import scala.collection.mutable.Buffer
import javax.imageio.ImageIO
import java.io.File
object mt {
  def sin(ar: Double) = {
    var arvo = ar
    while (arvo > 3.141593) arvo -= 6.283186
    while (arvo < -3.141593) arvo += 6.283186
    val kol = arvo * arvo * arvo
    val vii = kol * arvo * arvo
    val sei = vii * arvo * arvo
    arvo - ((kol) / 6) + (vii / 120) - (sei / 5040) + ((sei * arvo * arvo) / 362880)
  }

  def cos(ar: Double) = {
    var arvo = ar
    while (arvo > 3.141593) arvo -= 6.283186
    while (arvo < -3.141593) arvo += 6.283186
    val kak = arvo * arvo
    val nel = kak * arvo * arvo
    val kuu = nel * arvo * arvo
    val kah = kuu * arvo * arvo
    1 - ((kak) / 2) + (nel / 24) - (kuu / 720) + (kah / 40320)
  }
}

object GameInst extends scala.swing.Component {
  //load map data
  var areadata = Array.fill(10, 10)((0, 0, 0, 0))
  areadata(0)(0) = (0, 0, 2, 2)
  areadata(1)(0) = (2, 1, 2, 1)
  areadata(2)(0) = (2, 1, 1, 2)
  areadata(2)(1) = (1, 2, 1, 2)
  areadata(2)(2) = (2, 2, 1, 1)
  areadata(1)(2) = (2, 1, 2, 1)
  areadata(0)(2) = (1, 2, 2, 1)
  areadata(0)(1) = (1, 2, 1, 2)
  val texture = Array(ImageIO.read(new File("tex.png")), ImageIO.read(new File("pic.png")))
  var keyBuffer = Buffer[Key.Value]()
  // initial player location
  var py = 900
  var px = 300
  var prot = 0.0
  // our image to draw to
  var res = new BufferedImage(GAMED.w, GAMED.h, BufferedImage.TYPE_INT_ARGB)
  var g = res.getGraphics.asInstanceOf[Graphics2D]

  def keyU(key: Key.Value) = {
    keyBuffer -= key
  }

  def keyD(key: Key.Value) = {
    if (!keyBuffer.contains(key)) keyBuffer += key
  }

  // do we have a wall at point x,y? if so, what type
  def wallat(mx: Int, my: Int, x: Int, y: Int, s: Int, d: Int): (Boolean, Int, Int) = {
    val walls = areadata(mx)(my)
    val fx = mx * 600
    val fy = my * 600
    if (((x == fx + 600) & walls._3 != 2) & y > fy & y < fy + 600) return (true, (600 - (y - fy)), walls._3)
    if (((x == fx) & walls._1 != 2) & y > fy & y < fy + 600) return (true, (y - fy), walls._1)
    if (((y == fy + 600) & walls._4 != 2) & x > fx & x < fx + 600) return (true, (x - fx), walls._4)
    if (((y == fy) & walls._2 != 2) & x > fx & x < fx + 600) return (true, (600 - (x - fx)), walls._2)
    if (((x == fx + 601) & walls._3 != 2) & y > fy - 1 & y < fy + 601) return (true, (600 - (y - fy)), walls._3)
    if (((x == fx + 1) & walls._1 != 2) & y > fy & y < fy - 1 + 601) return (true, (y - fy), walls._1)
    if (((y == fy + 601) & walls._4 != 2) & x > fx - 1 & x < fx + 601) return (true, (x - fx), walls._4)
    if (((y == fy + 1) & walls._2 != 2) & x > fx & x < fx - 1 + 601) return (true, (600 - (x - fx)), walls._2)
    if (d == 0) return (false, 0, 0)
    if (walls._1 == 2 & s != 1) {
      val a = wallat(mx - 1, my, x, y, 3, d - 1)
      if (a._1) return (true, a._2, a._3)
    }
    if (walls._2 == 2 & s != 2) {
      val a = wallat(mx, my - 1, x, y, 4, d - 1)
      if (a._1) return (true, a._2, a._3)
    }
    if (walls._3 == 2 & s != 3) {
      val a = wallat(mx + 1, my, x, y, 1, d - 1)
      if (a._1) return (true, a._2, a._3)
    }
    if (walls._4 == 2 & s != 4) {
      val a = wallat(mx, my + 1, x, y, 2, d - 1)
      if (a._1) return (true, a._2, a._3)
    }
    return (false, 0, 0)
  }

  // placeholder for adding wall textures
  def getWallTex(a: Int): Int = {
    if (a == 1) return 1
    else return 0
  }

  // create an image
  def thisFrame() = {
    res = new BufferedImage(GAMED.w, GAMED.h, BufferedImage.TYPE_INT_ARGB)
    g = res.getGraphics.asInstanceOf[Graphics2D]
    g.setColor(java.awt.Color.GRAY)
    g.fillRect(0, 300, 800, 300)
    val lines = 400
    // get what map area we are in
    val mx = px / 600
    val my = py / 600
    val fx = mx * 600
    val fy = my * 600
    val walls = areadata(mx)(my)
    if (walls._1 == 1 & px < mx + 30) px += 30
    else if (walls._2 == 1 & py < fy + 30) py += 30
    else if (walls._3 == 1 & px > fx + 570) px -= 30
    else if (walls._4 == 1 & py > fy + 570) py -= 30
    // select column raycasting
    for (scan <- -lines / 2 to lines / 2) {
      val step = (math.Pi / 2) / lines
      val crot = step * scan + prot
      var dist = 0
      // cast a ray
      while (dist < 2650) {
        dist += 2
        val x = px + (dist * mt.sin(crot)).toInt
        val y = py + (dist * mt.cos(crot)).toInt
        val a = wallat(mx, my, x, y, 0, 3)
        if (a._1) {
          val fixeDist = dist * mt.cos(scan * step)
          // calculate height for wall
          val dra = ((64 * (400 / math.tan(math.Pi / 4))) / (fixeDist)).toInt
          // draw slice of the wanted wall texture on the wall
          g.drawImage(texture(a._3).getSubimage(math.abs(a._2) % 128, 0, 1, 32), scan * 2 + lines, 300 - dra.toInt, 2, 2 * dra.toInt, null)
          dist = 9000
        }
      }
    }
    res
  }
  // handle user input
  def processInput() = {
    if (keyBuffer.contains(Key.Up)) {
      px += (GAMED.speed * mt.sin(prot)).toInt
      py += (GAMED.speed * mt.cos(prot)).toInt
    } else if (keyBuffer.contains(Key.Down)) {
      px -= (GAMED.speed * mt.sin(prot)).toInt
      py -= (GAMED.speed * mt.cos(prot)).toInt
    }
    if (keyBuffer.contains(Key.Left)) {
      prot -= GAMED.rspeed
    } else if (keyBuffer.contains(Key.Right)) {
      prot += GAMED.rspeed
    }
    if (prot < -math.Pi) prot += math.Pi * 2
    else if (prot > math.Pi) prot -= math.Pi * 2
    if (px < 0) px = 0
    if (py < 0) py = 0
  }
}