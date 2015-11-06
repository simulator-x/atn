/*
 * Copyright 2015 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.ai.atn.elements

import simx.components.ai.atn.Events
import simx.components.ai.atn.core._
import simx.components.ai.atn.elements.Atn._
import simx.core.ontology.types
import simx.core.worldinterface.eventhandling.Event
import scala.collection.mutable
import scala.util.control.Breaks._
import simx.components.ai.atn.Functions._

/**
 * Created by chris on 01/08/15.
 */
class Layout() extends Functions {

  var network: Network = new Network()
  var arcs: mutable.HashMap[Symbol, Arc] = new mutable.HashMap()
  var mainNetwork: Option[SubNetwork] = None
  var subNetworks: List[SubNetwork] = Nil

  def states: Set[State] = {
    var result: Set[State] = Set()
    subNetworks.foreach(net => result ++= net.getStates)
    result
  }

  def states(withArc: Symbol):Set[State]= {
    network.get().filter(_._2.map(_._1.id).contains(withArc)).keySet.toSet
  }

  def getState(stateID: Symbol): State = {
    states.filter(_.id == stateID).head
  }

  def isSubStartState(stateID: Symbol): Boolean = {
    var res = false
    subNetworks.foreach{sub => if(sub.isStartState(stateID)) res = true}
    res
  }


  def getTransitions(state: State): List[(Arc, State)] = {
    network.get().get(state) match {
      case Some(trans) => trans
      case None => Nil
    }
  }

  private val _network = network.get()


  /**
   * this Method should invoke all functions needed to transform the network made by the user to the final
   * version needed by the atn machine
   * @return a reworked atn ready for action
   */
  def extendLayout(): Layout = {
    extendSplitMerge()
    subNetworks = disintegrate(network)
    subNetworks.foreach(net => if(net.isMainNet) mainNetwork = Some(net))
    if(debug) subNetworks.foreach(println)
    this
  }

  /**
   * this method creates a sub atn for every split-merge
   * and sets the needed conditions and functions to the arcs in order for the split-merge to work
   */
  private def extendSplitMerge() {
    val tempNet = network.get().clone()
    val tempArc = arcs.clone()
    _network.keySet.foreach{splitState =>
      if(splitState.isSplit){
        val result = findMergeTo(splitState, _network)
        if(result.isValid){

          def getSplitName(name: Symbol, prefix: String = "") = Symbol(prefix + "do-" + name.name)

          deleteAffectedParts(result.startState, result.allStates, tempNet)

          val subState = registerState(getSplitName(result.startState.id), tempNet)

          val endState = registerState(getSplitName(result.endState.get.id), tempNet)
          val endMerge = registerState(Symbol(splitState.id.name + "-completed"), tempNet)
          endMerge.asEnd()
          val mergeArc = registerArc(Symbol("mergOf-" + splitState.id.name), tempArc)
          mergeArc.isEpsilonArc = true
          mergeArc.arcType = Epsilon()
          addTransition(endState, mergeArc, endMerge, tempNet)



          val subStartState = registerState(getSplitName(result.startState.id, "start-"), tempNet)
          val toSplitStart = registerArc(Symbol("toSplit-" + result.startState.id), tempArc)
          toSplitStart.isEpsilonArc = true
          toSplitStart.arcType = Epsilon()
          addTransition(subState, toSplitStart, subStartState, tempNet)
          addConditions(toSplitStart :: Nil, leaveCursorBehind _ :: Nil)
          addFunctions(toSplitStart :: Nil, copyCompleteRegister _ :: Nil)




          replaceState(result.startState, subStartState, result.subNetwork)
          replaceState(result.endState.get, endState, result.subNetwork)

          val hiddenSplit = registerArc(Symbol("hidden-" + splitState.id.name), tempArc)
          addTransition(subStartState, hiddenSplit, subStartState, tempNet)

          // first merge then add subArc -> important for traversing order
          mergeNetworks(tempNet, result.subNetwork)
          val subArc = registerArc(getSplitName(result.startState.id), tempArc)
          subArc.isSubArc = true
          subArc.arcType = Sub()
          addTransition(result.startState, subArc, result.endState.get, tempNet)

          addConditions(hiddenSplit :: mergeArc :: result.allTransitions.map(_._1), splitState.conditions)
          addFunctions(hiddenSplit  :: result.allTransitions.map(_._1), registerTime _  :: Nil)

          addFunctions(mergeArc :: Nil, result.endState.get.functions)

          addConditions(mergeArc :: Nil, isSuccessfulMerge(result.incomingArcs)_ :: Nil)
          addFunctions(mergeArc :: Nil, doSuccessfullMerge(result.allStates)_ :: Nil)

          result.incomingArcs.foreach(arc => arc.functions ::= arrivedAtMerge(arc.id))

          addConditions(result.outgoingArcs, canGoFromSplit _ :: Nil)
          addFunctions(result.outgoingArcs, wentFromSplit _ :: Nil)

          addConditions(hiddenSplit :: Nil, checkForHiddenSplit(result.outgoingArcs)_ :: Nil)


        }
      }
    }
    arcs = tempArc
    network.set(tempNet)
  }




  private def copyCompleteRegister(in: Event, triggeredArc: ArcRep, curReg: StateRep, prevReg: StateRep, atn: ATNMachine): List[Event] = {
    prevReg.register.values.flatten.foreach{value => curReg.register.add(value)}
    Nil
  }



  private var searchableNetwork:mutable.HashMap[State, List[State]] = new mutable.HashMap()

  /**
   * disintegrates a given network into subnetworks
   * @param net network to be disintegrated
   * @return
   */
  private def disintegrate(net: Network): List[SubNetwork] ={
    searchableNetwork = prepareNetworkForGraphSearch(net)
    var networks: List[SubNetwork] = Nil
    var graphs: List[(mutable.HashMap[State, List[(Arc, State)]], State)] = Nil
    var helperNetwork = net.get()
    while (helperNetwork.nonEmpty) {

      val states = breadthFirstSearch(helperNetwork.keySet.head)
      helperNetwork = helperNetwork.filterNot(entry => states.contains(entry._1))
      val graph = net.get().filter(entry => states.contains(entry._1))
      val startOfGraph = graph.keySet.find(state => arcs.keySet.contains(state.id)).getOrElse(graph.keySet.filter(_.isStart).head)
      graphs = (graph, startOfGraph) :: graphs
    }
    graphs.foreach(graph => networks ::= new SubNetwork(graph._1, graph._2))
    networks
  }

  private def prepareNetworkForGraphSearch(net: Network):mutable.HashMap[State, List[State]]={
    val modifiedNetwork: mutable.HashMap[State, List[State]] = new mutable.HashMap()
    net.get().foreach(entry => {
      if (!modifiedNetwork.contains(entry._1)) {
        var list: List[State] = Nil
        entry._2.foreach(tuple => list = tuple._2 :: list)
        modifiedNetwork += (entry._1 -> list)
      }
    })
    modifiedNetwork.foreach(entry => {
      entry._2.foreach(state => {
        modifiedNetwork.get(state) match {
          case Some(list) =>
            var newList = list
            if (!newList.contains(entry._1)) {
              newList = entry._1 :: newList
            }
            modifiedNetwork += (state -> newList)
          case None => modifiedNetwork += (state -> List())
        }
      })
    })
    modifiedNetwork
  }

  private def breadthFirstSearch(startState: State): List[State] = {

    def expand(state: State): List[State] = {
      searchableNetwork.get(state).get
    }

    var network: List[State] = Nil
    var queue: mutable.Queue[State] = mutable.Queue()
    startState.visited = true
    queue += startState
    while (queue.nonEmpty) {
      val state = queue.dequeue()
      network = state :: network
      expand(state).foreach(child => {
        if (!child.visited) {
          queue += child
          child.visited = true
        }
      })
    }
    network
  }










  override def toString: String = network.toString

  private def addFunctions(toArcs: List[Arc], functions: List[ArcFunction.FunctionType]): Unit ={
    functions.foreach(function => toArcs.foreach(_.functions ::= function))
  }

  private def addConditions(toArcs: List[Arc], conditions: List[ArcCondition.FunctionType]): Unit ={
    conditions.foreach(condition => toArcs.foreach(_.conditions ::= condition))
  }

  private def deleteAffectedParts(startState: State, affectedStates: List[State], network: mutable.HashMap[State, List[(Arc, State)]]) = {
    network.remove(startState)
    network += (startState -> Nil)
    affectedStates.foreach(network.remove)
  }

  private def replaceState(original: State, newState: State, network: mutable.HashMap[State, List[(Arc, State)]]): Unit ={
    network.foreach{entry =>
      val list = network(entry._1)
      var newList: List[(Arc, State)] = Nil
      list.foreach(e => if (e._2 == original) newList ::=(e._1, newState) else newList ::= e)
      network += (entry._1 -> newList)
      if(entry._1 == original){
        val list = network(entry._1)
        network.remove(entry._1)
        network += (newState -> list)
      }
    }
  }

  private def mergeNetworks(n1: mutable.HashMap[State, List[(Arc, State)]], n2: mutable.HashMap[State, List[(Arc, State)]]) {
    n2.foreach(entry => entry._2.foreach(transition => addTransition(entry._1, transition._1, transition._2, n1)))
  }

  private def addTransition(startState: State, arc: Arc, targetState: State, network: mutable.HashMap[State, List[(Arc, State)]]): Unit = {
    var transitions = network.getOrElse(startState, Nil)
    transitions ::= (arc, targetState)
    network += (startState -> transitions)
  }

  private def checkForHiddenSplit(outgoingArcs: List[Arc])(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): Condition.Result = {
    var result = true
    if (Atn.debug) {
      println("[ATN][DEBUG]------Check for hidden split------")
    }
    val outWentArcs = curState.register.getAllValuesFor(mergeIdentifier)
    result = !compare(outWentArcs, outgoingArcs.map(arc => arc.id))
    if (Atn.debug) {
      println("[ATN][DEBUG] Register: " + outWentArcs + " from State " + curState.id)
      println("[ATN][DEBUG] For successful merge has to be equal to  " + outgoingArcs.map(arc => arc.id))
      println("[ATN][DEBUG] Condition for hidden split transition = " + result)
      println("[ATN][DEBUG]-----------------------------------")
    }
    (result, Nil, KeepCursor())
  }

  private def leaveCursorBehind(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): Condition.Result = {
    var result = true
    var command: AtnResult = LeaveCursorBehind()
    if (curState.register.values.flatten.nonEmpty) {
      command = KeepCursor()
      result = false
    }
    curState.register.add(types.AudioFile("  "))
    (result, Nil, command)
  }


  private def compare(list1: List[Symbol], list2: List[Symbol]): Boolean = {
    list1.toSet == list2.toSet
  }

  private def isSuccessfulMerge(incomingArcs: List[Arc])(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): Condition.Result = {
    val arcs = prevState.register.getAllValuesFor(mergeIdentifier)
    var result = true
    var atnResult: AtnResult = DisableCursor()
    incomingArcs.map(a => a.id).foreach(arc => if (!arcs.contains(arc)) result = false)
    if (Atn.debug) {
      println("[ATN][DEBUG]------isSuccessfullMerge-----")
      println("[ATN][DEBUG] Register: " + arcs + " from State " + prevState.id)
      println("[ATN][DEBUG] Should be equal to this: " + incomingArcs.map(_.id))
      println("[ATN][DEBUG] Result: " + result)
      println("[ATN][DEBUG] ----------------------------")
    }
    if (result) atnResult = ResetCurrentNetwork()
    (result, Nil, atnResult)
  }


  private def doSuccessfullMerge(allStates: List[State])(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): List[Event] = {
    def deleteResidualCursors(stateID: Symbol): Unit ={
      atn.atnLayout.subNetworks.foreach(sub =>
        if(sub.contains(curState.id)) {
          deleteHigherCursor(sub.startState.id)
        }
      )
    }
    def deleteHigherCursor(stateID: Symbol): Unit ={
      atn.atnLayout.states(stateID).foreach {state =>
        atn.deleteCursor(state.id)
        if(isSubStartState(state.id)) deleteHigherCursor(state.id)
      }
    }
    deleteResidualCursors(curState.id)
    Nil
  }

  private def registerTime(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): List[Event] = {
    curState.register += mergeTimestamp(in.values.firstSValFor(types.Time).value)
    Nil
  }
  case class MergeNotFoundException() extends Exception

  case class SplitInSplitException() extends Exception

  case class SplitMergeResult(isValid: Boolean,
                              startState: State,
                              endState: Option[State],
                              allTransitions: List[(Arc, State)],
                              subNetwork: mutable.HashMap[State, List[(Arc, State)]],
                              allStates: List[State],
                              outgoingArcs: List[Arc],
                              incomingArcs: List[Arc])

  private def findMergeTo(splitState: State, network: mutable.HashMap[State, List[(Arc, State)]]): SplitMergeResult = {

    var success: Boolean = false
    var foundArcs: List[(Arc, State)] = Nil
    var foundStates: List[State] = Nil
    var foundMerge: Option[State] = None
    var recentArcs: List[(Arc, State)] = Nil
    var incomingArcs: List[Arc] = Nil
    var allStates: List[State] = Nil
    val outgoingArcs: List[Arc] = network.get(splitState).map(list => list.map(tupel => tupel._1)).getOrElse(Nil)
    val subNetwork: mutable.HashMap[State, List[(Arc, State)]] = new mutable.HashMap()

    var queue: mutable.Queue[State] = new mutable.Queue[State]()

    queue += splitState
    foundStates = splitState :: foundStates

    breakable {
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        if (node.isMerge) {
          success = true
          foundMerge = Some(node)
          foundStates = node :: foundStates
          break()
        }
        if (success && node.isSplit) throw new SplitInSplitException()
        recentArcs = Nil
        network(node).foreach(transition => {
          if (!transition._1.isHiddenSplit) {
            recentArcs = transition :: recentArcs
            foundArcs = transition :: foundArcs
            if (transition._2.isMerge) incomingArcs ::= transition._1
            queue += transition._2
            if (!foundStates.contains(node)) foundStates = node :: foundStates
          }
        })
        subNetwork += (node -> recentArcs)
      }
    }
    if (success) {
      allStates = foundStates.filterNot(state => state.hiddenID == splitState.hiddenID || state.hiddenID == foundMerge.get.hiddenID)
      if (Atn.debug) {
        println("[ATN][DEBUG]----------------Split Merge Resolver-------------------")
        println("[ATN][DEBUG] Split state: " + splitState.id)
        println("[ATN][DEBUG] Merge state: " + foundMerge.map(_.id).getOrElse("not found"))
        println("[ATN][DEBUG] All states: " + foundStates.map(_.id))
        println("[ATN][DEBUG] Found states between: " + allStates.map(_.id))
        println("[ATN][DEBUG] Found arcs between: " + foundArcs.map(_._1.id))
        println("[ATN][DEBUG] Incoming arcs to merge: " + incomingArcs.map(_.id))
        println("[ATN][DEBUG] Outgoing arcs from split: " + outgoingArcs.map(_.id))
        println("[ATN][DEBUG] Found SubnetWork: " + subNetwork)
        println("[ATN][DEBUG]-------------------------------------------------------")
      }
    } else throw new MergeNotFoundException()
    new SplitMergeResult(success, splitState, foundMerge, foundArcs, subNetwork, allStates, outgoingArcs, incomingArcs)
  }


  /*
  ----------------------------------------
   */

  private def arrivedAtMerge(arcID: Symbol)(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): List[Event] = {
    curState.register += mergeIdentifier(arcID)
    //println("[DEBUG] Doing arrivedAtMerge with " + arcID + " @" + curState)
    Nil
  }

  // Ignore the problem of multiple hiddenTransitions and just let every outgoing arc of a split out only once
  private def canGoFromSplit(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): Condition.Result = {
    val alreadyWent = prevState.register.getAllValuesFor(mergeIdentifier)
    val result = !alreadyWent.contains(triggeredArc.name)
    if (Atn.debug) {
      println("[ATN][DEBUG]--"+triggeredArc.name+" can go from split ?--")
      println("[ATN][DEBUG] Result: " + result)
      println("[ATN][DEBUG]--------------------------")
    }
    (result, Nil, KeepCursor())
  }

  private def wentFromSplit(in: Event, triggeredArc: ArcRep, curState: StateRep, prevState: StateRep, atn: ATNMachine): List[Event] = {
    prevState.register += mergeIdentifier(triggeredArc.name)
    //    if (Atn.debug) {
    //      println("[ATN][DEBUG]------Went from split-----")
    //      println("[ATN][DEBUG] Register: " + prevState.register + " from State " + prevState.id)
    //      println("[ATN][DEBUG]--------------------------")
    //    }
    Nil
  }

}