package simx.components.ai.atn.gui

import scala.collection.mutable
import scala.swing.event.ButtonClicked
import java.util.UUID
import simx.components.ai.atn.elements._


class Control(layout: Layout) {

  private val network = layout.network.get()
  private val arcNames = layout.arcs.keySet.toSet
  private var arcMemory: List[String] = Nil


  val stringGraphs = convertGraphsToString(layout.subNetworks)

  private val startState = network.keySet.find(state => state.isStart).get
  private val endStates = network.keySet.filter(state => state.isEnd)
  private val subStartStates = network.keySet.filter(state => layout.arcs.keySet.contains(state.id))
  private var endStatesStrings : List[String] = Nil
  endStates.foreach(state => endStatesStrings = state.id.toString().drop(1) :: endStatesStrings)
  private var subStartStateStrings : List[String] = Nil
  subStartStates.foreach(state => subStartStateStrings = state.id.toString().drop(1) :: subStartStateStrings)

  var graphContainer = createGraphContainer()

  private var graphViews: List[GraphView] = Nil

  def displayGUI(){
    stringGraphs.foreach(graph =>
      graphViews = new GraphView(graph, startState.id.toString().drop(1),subStartStateStrings, endStatesStrings) :: graphViews)

    val view = new MainView(stringGraphs)

    view.buttons.foreach(button => {
      view.listenTo(button)
      view.reactions += {
        case ButtonClicked(`button`) => graphViews.find(graph => graph.title == button.text).get.visible = true
      }
    })
  }

  def updateGui(cursors: List[Cursor]) {
    graphViews.foreach(gV => gV.highlightCurrentCursors(cursors))
  }

  def updatedGraphContainer(currentCursor : List[Cursor]){
    val newGraphContainer :  mutable.HashMap[String, GraphContainer] = new mutable.HashMap[String, GraphContainer]
    graphContainer.foreach(gC => {
      val newContainer = new GraphContainer
      newContainer.fillContainer(gC._2.id.get, gC._2.graph.get, gC._2.startState.get, gC._2.subStartStates.get, gC._2.endStates.get)
      newContainer.updateCurrentCursor(currentCursor)
      newGraphContainer +=  (gC._1 -> newContainer)
    })
    graphContainer = newGraphContainer
  }


  /**
   * converts Graphs to String Representation for JUNG Framework
   */

  private def convertGraphsToString(_graphs: List[Network]): mutable.HashMap[String, mutable.HashMap[String, List[(String, String)]]] = {
    val graphs = _graphs.map(_.get())
    var modifiedGraphs: mutable.HashMap[String, mutable.HashMap[String, List[(String, String)]]] = mutable.HashMap()
    graphs.foreach(graph => {
      val name = getGraphName(graph)
      modifiedGraphs += (name -> convertGraphToString(graph))
    })
    modifiedGraphs
  }

  private def convertGraphToString(graph: mutable.HashMap[State, List[(Arc, State)]]): mutable.HashMap[String, List[(String, String)]] = {
    var convertedGraph: mutable.HashMap[String, List[(String, String)]] = mutable.HashMap()
    graph.foreach(entry => {
      var convertedList: List[(String, String)] = Nil
      entry._2.foreach(tuple => {
        convertedList = (generateArcName(tuple._1.id.toString().drop(1), 1), tuple._2.id.toString().drop(1)) :: convertedList
      })
      convertedGraph += (entry._1.id.toString().drop(1) -> convertedList)
    })
    convertedGraph
  }

  private def getGraphName(graph: mutable.HashMap[State, List[(Arc, State)]]): String = {
    graph.keySet.find(state => arcNames.contains(state.id)) match {
      case Some(state) => state.id.toString().drop(1)
      case None => "Start"
    }
  }

  private def generateArcName(id: String, counter: Int): String = {
    val newID = id + "(" + counter + ")"
    if (!arcMemory.contains(newID)) {
      arcMemory = newID :: arcMemory
      if (counter == 1) {
        id
      } else {
        newID
      }
    } else {
      val newCounter = counter + 1
      generateArcName(id, newCounter)
    }
  }


  /**
   * creates Container Class for Entities
   * */
  private def createGraphContainer(): mutable.HashMap[String, GraphContainer] ={
    var graphContainer = mutable.HashMap[String, GraphContainer]()
    stringGraphs.foreach(graph => {
      val con = new GraphContainer()
      graphContainer += (graph._1 -> con)
      con.fillContainer(graph._1, graph._2, startState.id.name, subStartStateStrings, endStatesStrings)
    })
    graphContainer
  }

}

case class GraphContainer(uuID : UUID = UUID.randomUUID()) {

  var id : Option[String] = None
  var graph : Option[mutable.HashMap[String, List[(String, String)]]] = None
  var startState : Option[String] = None
  var subStartStates : Option[List[String]] = None
  var endStates : Option[List[String]] = None
  var currentCursor : Option[List[Cursor]] = None


  def fillContainer(idX : String,
                    graphX : mutable.HashMap[String, List[(String, String)]],
                    startStateX : String,
                    subStartStatesX : List[String],
                    endStatesX : List[String]){

    id = Some(idX)
    graph = Some(graphX)
    startState = Some(startStateX)
    subStartStates = Some(subStartStatesX)
    endStates = Some(endStatesX)
  }



  def updateCurrentCursor(newCurrentCursor : List[Cursor]){
    currentCursor = Some(newCurrentCursor)
  }
}
