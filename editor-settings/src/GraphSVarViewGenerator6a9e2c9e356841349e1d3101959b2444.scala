//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import edu.uci.ics.jung.graph.DirectedSparseMultigraph
import edu.uci.ics.jung.algorithms.layout.{FRLayout, CircleLayout}
import edu.uci.ics.jung.visualization.{GraphZoomScrollPane, VisualizationViewer}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, AbstractModalGraphMouse}
import scala.collection.mutable
import simx.components.ai.atn.gui.GraphContainer
import org.apache.commons.collections15.Transformer
import java.awt.{Color, Paint}
import scala.swing.Color

class GraphSVarViewGenerator6a9e2c9e356841349e1d3101959b2444 extends SVarViewGenerator[GraphContainer] {

  def generate: SVarView[GraphContainer] = new SVarView[GraphContainer] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the value.
     */

    //TODO: naja gridpanel is nicht so des wahre aber es klappt
    val component = new GridPanel(1,1)//Component.wrap(panel)

    /**
      *  This function is called whenever the visualized value changes.
      *  It should update component accordingly.
      */

    def update(sVarValue: GraphContainer) {
      val g = new DirectedSparseMultigraph[String, String] {}

      sVarValue.graph.get.foreach(entry => {
        g.addVertex(entry._1)
        entry._2.foreach(arcTarget => {
          g.addEdge(arcTarget._1, entry._1, arcTarget._2)
        })
      })

      val layout = new FRLayout(g)
      val vv = new VisualizationViewer[String, String](layout)
      vv.getRenderContext.setVertexLabelTransformer(new ToStringLabeller())
      vv.getRenderContext.setEdgeLabelTransformer(new ToStringLabeller[String])
      vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)
      val graphMouse: AbstractModalGraphMouse = new DefaultModalGraphMouse[Symbol, Symbol]()
      graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.PICKING)
      vv.setGraphMouse(graphMouse)
      val panel = new GraphZoomScrollPane(vv)

      val vertexColor : Transformer[String,Paint] = new Transformer[String,Paint]() {
        def transform(i : String):Paint={
          if(i == sVarValue.startState.get) {
            Color.GREEN
          } else if (sVarValue.endStates.get.contains(i)){
            Color.RED
          } else if (sVarValue.subStartStates.get.contains(i)) {
            Color.YELLOW
          } else {
            Color.WHITE
          }
        }
      }

      vv.getRenderContext.setVertexFillPaintTransformer(vertexColor)


      component.contents.clear()
      component.contents += Component.wrap(panel)

    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "GraphView"

}