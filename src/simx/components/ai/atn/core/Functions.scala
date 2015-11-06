package simx.components.ai.atn.core


import simx.components.ai.atn.elements.{Atn, Arc, State}
import simx.core.entity.description.SValSet
import simx.core.ontology.Symbols.debug
import simx.core.ontology.{Symbols, types}
import simx.core.worldinterface.eventhandling.Event
import scala.collection.mutable
import scala.util.control.Breaks._




trait Functions {

  import simx.components.ai.atn.elements.Atn._

  protected def registerState(stateID: Symbol, atnNetwork: mutable.HashMap[State, List[(Arc, State)]]): State = {

    atnNetwork.keySet.find(state => state.id == stateID) match {
      case None => {
        val newState = new State(stateID)
        val newList: List[(Arc, State)] = Nil
        atnNetwork += (newState -> newList)
        newState
      }
      case Some(state) => {
        state
      }
    }
  }

  protected def registerArc(arcID: Symbol, arcIndex: mutable.HashMap[Symbol, Arc]): Arc = {
    arcIndex.get(arcID) match {
      case None => {
        val newArc = new Arc(arcID)
        arcIndex += (arcID -> newArc)
        newArc
      }
      case Some(arc) => {
        arc
      }
    }
  }


  //  protected def postAtnCreation(network: mutable.HashMap[State, List[(Arc, State)]]) {
  //    //registerForAllArcs(registerTime _ :: Nil, network)
  //    prepareMergeSplitConditions(network)
  //  }

  //  private def registerForAllArcs(funcs: List[((Event, ArcRep, StateRep, StateRep, ATNMachine) => List[Event])], network: mutable.HashMap[State, List[(Arc, State)]]){
  //    network.foreach(entry => entry._2.foreach(trans => funcs.foreach(func => trans._1.functions ::= func)))
  //  }

  //  private def prepareMergeSplitConditions(network: mutable.HashMap[State, List[(Arc, State)]]) {
  //    network.keySet.foreach(state => {
  //      if(state.isSplit){
  //        var splitArc: Option[Arc] = None
  //        var outgoingArcs: List[Arc] = Nil
  //        //var conditions: List[(Event, StateRep, StateRep, ATNMachine) => Condition.Result] = Nil
  //        // adding the split condition from the split state to every arc
  //        val(foundMerge, foundStates, foundArcs, recentArcs) = findMergeTo(state, network)
  //        foundMerge.collect{case merge => {
  //          foundArcs.foreach(arc => {
  //            arc.conditions = state.conditions ::: arc.conditions
  //            arc.functions ::= registerTime
  //          })
  //          val mergeArc = network(merge).head._1
  //          mergeArc.conditions = isSuccessfullMerge(recentArcs)_ :: mergeArc.conditions
  //          mergeArc.conditions = state.conditions ::: mergeArc.conditions
  //          mergeArc.functions ::= doSuccessfullMerge(network.map(e=> e._1).toList)
  //          recentArcs.foreach(arc => arc.functions ::= arrivedAtMerge(arc.id))
  //        }}
  //
  //        // getting conditions from all outgoing arcs and adding them to splitTOArc
  //        network.get(state).collect{case list =>
  //          list.foreach(trans => {
  //           // conditions = trans._1.conditions ::: conditions
  //            if(trans._1.isHiddenSplit) splitArc = Some(trans._1)
  //            else {
  //              outgoingArcs = trans._1 :: outgoingArcs
  //              trans._1.functions ::= wentFromSplit
  //            }
  //          })
  //            if(debug){
  //              println("Hidden Split Arc: " + splitArc.map(_.id).getOrElse("not found"))
  //              println("Outgoing Arcs from SplitState: " + outgoingArcs.map(_.id))
  //              println("-----------------------------------")
  //            }
  //        }
  //        splitArc.foreach(arc => {
  //          arc.functions ::= registerTime
  //          arc.conditions = checkForHiddenSplit(outgoingArcs)_ :: arc.conditions
  //        })
  //        //outgoingArcs.foreach(arc => arc.conditions ::= alreadyTriggerdInMerge)
  //        //conditions = state.conditions ::: conditions
  //        //splitArc.foreach(arc => arc.conditions = conditions)
  //      }
  //    })
  //  }


  protected def frontEndRep(network: mutable.HashMap[State, List[(Arc, State)]]) = {
    def createFrontEndArcRep(arc: Arc) = {
      new ArcRep(arc.id)
    }

    def createFrontEndStateRep(state: State, arcReps: List[ArcRep]) = {
      new GeneralStateRep(
        id = state.id,
        register = state.register,
        arcs = arcReps
      )
    }
    var frontEndRepresentation: Map[Symbol, GeneralStateRep] = Map()
    network.foreach(entry => {
      var arcReps: List[ArcRep] = Nil
      entry._2.foreach(trans => arcReps = createFrontEndArcRep(trans._1) :: arcReps)
      frontEndRepresentation += (entry._1.id -> createFrontEndStateRep(entry._1, arcReps))
    })

    frontEndRepresentation
  }

  protected def extractInComingArcs(network: mutable.HashMap[State, List[(Arc, State)]]) = {
    val result: mutable.HashMap[State, List[Arc]] = mutable.HashMap()
    network.keySet.foreach(state => {
      var newList: List[Arc] = Nil
      network.values.foreach(list => list.foreach(trans => {
        if (trans._2 == state) newList = trans._1 :: newList
      }))
      result += (state -> newList)
    })
    result
  }

  protected def clearRegister(reg: SValSet) {
    reg - mergeIdentifier
    reg - mergeTimestamp
  }









}
