//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simx.components.ai.atn.gui.{VertexScalerFunction, GraphContainer}
import edu.uci.ics.jung.graph.DirectedSparseMultigraph
import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.visualization.{GraphZoomScrollPane, VisualizationViewer}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, AbstractModalGraphMouse}
import org.apache.commons.collections15.Transformer
import java.awt.{Shape, Color, Paint}
import java.awt.geom.{AffineTransform, Ellipse2D}

class GraphContainerSVarViewGeneratora758a4e1abc74c46a0172a3e52ecc4d9 extends SVarViewGenerator[GraphContainer] {

  def generate: SVarView[GraphContainer] = new SVarView[GraphContainer] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the value.
     */
    //Todo: Implement yourself!
    val component = new GridPanel(1,1)
    var firstTurn = true
    var g : DirectedSparseMultigraph[String, String] = null
    var vv : VisualizationViewer[String, String] = null


    /**
      *  This function is called whenever the visualized value changes.
      *  It should update component accordingly.
      */
    //Todo: Implement yourself!
    def update(sVarValue: GraphContainer) {

      if(firstTurn){
        g = new DirectedSparseMultigraph[String, String] {}

        sVarValue.graph.get.foreach(entry => {
          g.addVertex(entry._1)
          entry._2.foreach(arcTarget => {
            g.addEdge(arcTarget._1, entry._1, arcTarget._2)
          })
        })

        val layout = new FRLayout(g)
        vv = new VisualizationViewer[String, String](layout)

        vv.getRenderContext.setVertexLabelTransformer(new ToStringLabeller())
        vv.getRenderContext.setEdgeLabelTransformer(new ToStringLabeller[String])
        vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)
        val graphMouse: AbstractModalGraphMouse = new DefaultModalGraphMouse[Symbol, Symbol]()
        //graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.PICKING)
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
        vv.getRenderContext.setVertexShapeTransformer(new VertexScalerFunction[String](vv.getPickedVertexState))


        component.contents.clear()
        component.contents += Component.wrap(panel)

        firstTurn = false
      }




      vv.getPickedVertexState.clear()
      sVarValue.currentCursor.get.foreach(cursor => {
        val stateName = cursor.state.id.toString().drop(1)
        if(g.containsVertex(stateName)){vv.getPickedVertexState.pick(stateName, true)}
      })








    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "GraphContainer View"

}