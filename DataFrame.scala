package schelling

import java.awt.Color
import java.awt.Graphics2D
import swing._
import javax.swing.WindowConstants._

class DataGUI extends SimpleSwingApplication {

  val frame = new DataFrame 

  def top = frame

  def display = {
    frame.minimumSize = new Dimension(600, 600)
    frame.visible = true
  }

}

class DataFrame extends MainFrame {
  val p = new DataPanel {
    preferredSize = new Dimension(600, 600)
  }
  
  def update(data: Seq[Seq[Color]]) = { 
    p.data = data 
    p.revalidate
    p.repaint
  }

  contents = p
}

class DataPanel(var _data: Seq[Seq[Color]] = Seq.empty) extends Panel {

  def data_=(data: Seq[Seq[Color]]) = synchronized { _data = data }
  def data = _data

  override def paintComponent(g: Graphics2D) = synchronized {
    if(!data.isEmpty) {
      val dx = g.getClipBounds.width.toFloat  / data.length
      val dy = g.getClipBounds.height.toFloat / data.map(_.length).max
      for {
        x <- 0 until data.length
        y <- 0 until data(x).length
        x1 = (x * dx).toInt
        y1 = (y * dy).toInt
        x2 = ((x + 1) * dx).toInt
        y2 = ((y + 1) * dy).toInt
      } {
        g.setColor(data(x)(y))
        g.fillRect(x1, y1, x2 - x1, y2 - y1)
      }
    }
  }
}
