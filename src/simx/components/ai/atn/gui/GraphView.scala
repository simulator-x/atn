package simx.components.ai.atn.gui

import scala.swing.{Component, Frame}
import scala.collection.mutable
import edu.uci.ics.jung.graph.DirectedSparseMultigraph
import edu.uci.ics.jung.algorithms.layout.{FRLayout, CircleLayout}
import edu.uci.ics.jung.visualization.{GraphZoomScrollPane, VisualizationViewer}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, AbstractModalGraphMouse}
import java.awt._
import org.apache.commons.collections15.Transformer
import edu.uci.ics.jung.visualization.picking.PickedInfo
import scala.List
import simx.components.ai.atn.elements.Cursor
import java.awt.geom.{AffineTransform, Ellipse2D}


case class GraphView(graph: (String, mutable.HashMap[String, List[(String, String)]]),
                      startState: String,
                      subStartStates: List[String],
                      endStates: List[String]) extends Frame {

  title = graph._1
  pack()


  private val g = new DirectedSparseMultigraph[String, String] {}
  graph._2.foreach(entry => {
    g.addVertex(entry._1)
    entry._2.foreach(arcTarget => {
      g.addEdge(arcTarget._1, entry._1, arcTarget._2)
    })
  })


  private val layout = new FRLayout(g)
  private val vv = new VisualizationViewer[String, String](layout)
  vv.getRenderContext.setVertexLabelTransformer(new ToStringLabeller())
  vv.getRenderContext.setEdgeLabelTransformer(new ToStringLabeller[String])
  vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)
  private val graphMouse: AbstractModalGraphMouse = new DefaultModalGraphMouse[Symbol, Symbol]()
  //graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.PICKING)

  vv.setGraphMouse(graphMouse)

  //vv.getRenderContext.setVertexFillPaintTransformer(new VertexPainterFunction[String](vv.getPickedVertexState))

  var vertexColor : Transformer[String,Paint] = new Transformer[String,Paint]() {
    def transform(i : String):Paint={
      if(i == startState) {
        Color.GREEN
      } else if (endStates.contains(i)){
        Color.RED
      } else if (subStartStates.contains(i)) {
        Color.YELLOW
      } else {
        Color.WHITE
      }
    }
  }

  vv.getRenderContext.setVertexFillPaintTransformer(vertexColor)

  vv.getRenderContext.setVertexShapeTransformer(new VertexScalerFunction[String](vv.getPickedVertexState))

  private val panel = new GraphZoomScrollPane(vv)

  contents = Component.wrap(panel)

  centerOnScreen()
  visible = false



  def highlightCurrentCursors(currentCursors: List[Cursor]){
    vv.getPickedVertexState.clear()
    currentCursors.foreach(cursor => {
      val stateName = cursor.state.id.toString().drop(1)
      if(g.containsVertex(stateName)){vv.getPickedVertexState.pick(stateName, true)}
    })
  }

}

class VertexScalerFunction[V](_pickedInfo : PickedInfo[V]) extends Transformer [V, Shape] {

  var pickedInfo : PickedInfo[V] = _pickedInfo

  def transform(p1: V): Shape = {
    val circle : Ellipse2D = new Ellipse2D.Double(-10, -10, 25, 25)
    if(pickedInfo.isPicked(p1)) {
       AffineTransform.getScaleInstance(2, 2).createTransformedShape(circle)
    }
    else {
       circle
    }

  }
}


class VertexPainterFunction[V](_pickedInfo : PickedInfo[V]) extends Transformer [V, Paint] {

  var pickedInfo : PickedInfo[V] = _pickedInfo


  def transform(p1: V): Paint = {
    if(pickedInfo.isPicked(p1)){
      Color.YELLOW
    } else {
      Color.CYAN
    }
  }
}


