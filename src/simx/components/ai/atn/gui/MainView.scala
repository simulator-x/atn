package simx.components.ai.atn.gui

import scala.swing._
import scala.collection.mutable


class MainView(modifiedGraphs: mutable.HashMap[String, mutable.HashMap[String, List[(String, String)]]]) extends Frame {
  title = "Graphs"
  var buttons: List[Button] = Nil


  val gridLayout = new GridPanel(modifiedGraphs.keySet.size, 1) {
    modifiedGraphs.keySet.foreach(name => {
      val button = new Button(name)
      buttons = button :: buttons
      contents += button
    })
  }


  contents = new BorderPanel {
    add(gridLayout, BorderPanel.Position.Center)
  }

  minimumSize = new Dimension(400, 300)
  visible = true


}


