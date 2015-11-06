package simx.components.ai.atn.core

import java.util.UUID

import simx.components.ai.atn.{Events, ImplicitConversions}
import simx.components.ai.atn.elements._
import simx.core.entity.description.SValSet
import simx.core.ontology._
import simx.core.worldinterface.eventhandling.{Event, EventDescription}

import scala.collection.mutable


trait AugmentedTransitionNetwork extends Functions {

  def atn = this

  private val layout = new Layout()
  def getLayout = layout.extendLayout()

  def create = new create(layout.network.get(), layout.arcs)

  val cursorMerge : Boolean
  val inputTypes: List[EventDescription]
  val outputTypes: List[EventDescription]
}




case class create(atnNetwork: mutable.HashMap[State, List[(Arc, State)]],
                  arcIndex: mutable.HashMap[Symbol, Arc]) extends Functions {

  def StartState(stateID: Symbol): WithArc = {
    val state = registerState(stateID, atnNetwork)
    state.asStart()
    new WithArc(state, atnNetwork, arcIndex)
  }

  def State(stateID: Symbol): WithArc = {
    val state = registerState(stateID, atnNetwork)
    new WithArc(state, atnNetwork, arcIndex)
  }

  def Split(stateID: Symbol): WithCondition = {
    val state = registerState(stateID, atnNetwork)
    state.asSplit()
    new WithCondition(state, atnNetwork, arcIndex)
  }

  def EndState(stateID: Symbol): WithArc = {
    val state = registerState(stateID, atnNetwork)
    state.asEnd()
    new WithArc(state, atnNetwork, arcIndex)
  }

  def Merge(stateID: Symbol) = {
    val state = registerState(stateID, atnNetwork)
    state.asMerge()
    new {
      /**
       *  One function to decide how to merge (e.g copy register values from prev to cur)
       */
      def withOnMergeFunction(function: ArcFunction.FunctionType) = {
          state.functions = function :: state.functions
        new WithArc(state, atnNetwork, arcIndex) {
          def asEndState: Unit = {
            state.asEnd()
          }
        }
      }
    }
  }

  def Arc(arcID: Symbol): ArcCondition = {
    new ArcCondition(registerArc(arcID, arcIndex))
  }

  def EpsilonArc(arcID: Symbol): ArcCondition = {
    new ArcCondition(registerArc(arcID, arcIndex))
  }

}

abstract sealed class MergeCondition() extends ((Event, ArcRep, StateRep, StateRep, ATNMachine) => Condition.Result)

/**
 * Let's a merge only be successful if all paths from split to merge are successfully pursued within the given time frame maximumDeltaInMillis.
 * This is currently the only supported MergeCondition.
 */
class TimeCondition(maximumDeltaInMillis: Long) extends MergeCondition {
  override def apply(in: Event, triggeredArc: ArcRep, curReg: StateRep, prevReg: StateRep, atn: ATNMachine): Condition.Result = {
    //println("Called from " + triggeredArc.name)
    var oldTimeSVar = prevReg.register.getFirstValueFor(Atn.mergeTimestamp)
    val newTime = in.values.firstValueFor(types.Time)
    var cmd: AtnResult = KeepCursor()
    var res = false
    if(oldTimeSVar.isEmpty) {
      oldTimeSVar = Some(newTime)
    }
    oldTimeSVar.collect{case time => if((newTime - time) < maximumDeltaInMillis) res = true}
    if(!res) {
      cmd = RedoCurrentNetwork()
    }
    //println(cmd)
    var eventList: List[Event] = Nil
    if(!res) {
      eventList ::= Events.resetSubNetwork.createEvent(types.Identifier(prevReg.id))
    }
    ImplicitConversions.toTriple(ConditionResult(res, eventList, cursorAction = cmd))
  }
}

case class WithCondition(state: State,
                         atnNetwork: mutable.HashMap[State, List[(Arc, State)]],
                         arcIndex: mutable.HashMap[Symbol, Arc]) extends Functions {

  def withCondition(condition: MergeCondition):WithSplitBranch = {
    state.conditions = condition :: state.conditions
    new WithSplitBranch(state, atnNetwork, arcIndex)
  }
}

case class WithSplitBranch(state: State,
                           atnNetwork: mutable.HashMap[State, List[(Arc, State)]],
                           arcIndex: mutable.HashMap[Symbol, Arc]) extends Functions {

  def andArc(arcID: Symbol) = andBranch(arcID)
  def andBranch(arcID: Symbol) :SplitTarget = {
    val arcTarget = new WithArc(state, atnNetwork, arcIndex).withArc(arcID)
    new SplitTarget(arcTarget.arc, state, atnNetwork, arcIndex)
  }
}

case class WithArc(state: State,
                   atnNetwork: mutable.HashMap[State, List[(Arc, State)]],
                   arcIndex: mutable.HashMap[Symbol, Arc]) extends Functions {

  def andSubArc(arcID: Symbol) = withSubArc(arcID)
  def withSubArc(arcID: Symbol): ArcTarget = {
    val arc = registerArc(arcID, arcIndex)
    arc.isSubArc = true
    arc.arcType = Sub()
    new ArcTarget(arc, state, atnNetwork, arcIndex)
  }

  def andArc(arcID: Symbol) = withArc(arcID)
  def withArc(arcID: Symbol): ArcTarget = {
    val arc = registerArc(arcID, arcIndex)
    new ArcTarget(arc, state, atnNetwork, arcIndex)
  }

  def andEpsilonArc(arcID: Symbol) = withEpsilonArc(arcID)
  def withEpsilonArc(arcID: Symbol): ArcTarget = {
    val arc = registerArc(arcID, arcIndex)
    arc.isEpsilonArc = true
    arc.arcType = Epsilon()
    new ArcTarget(arc, state, atnNetwork, arcIndex)
  }
}

case class SplitTarget(arc: Arc,
                       state: State,
                       atnNetwork: mutable.HashMap[State, List[(Arc, State)]],
                       arcIndex: mutable.HashMap[Symbol, Arc]) extends Functions {

  def  toTargetState(targetStateID: Symbol): WithSplitBranch = {
    val withArc = new ArcTarget(arc, state, atnNetwork, arcIndex).toTargetState(targetStateID)
    new WithSplitBranch(state, atnNetwork, arcIndex)
  }
}

case class ArcTarget(arc: Arc,
                     state: State,
                     atnNetwork: mutable.HashMap[State, List[(Arc, State)]],
                     arcIndex: mutable.HashMap[Symbol, Arc]) extends Functions {

  def toTargetState(targetStateID: Symbol): WithArc = {
    val targetState = registerState(targetStateID, atnNetwork)
    val newList = (arc, targetState) :: atnNetwork.get(state).get
    atnNetwork -= state
    atnNetwork += (state -> newList)
    new WithArc(state, atnNetwork, arcIndex)
  }

}

object Condition {
  type Result = (Boolean, List[Event], AtnResult)
}

abstract class AtnResult
case class KeepCursor() extends AtnResult
case class ResetCurrentNetwork() extends AtnResult
case class RedoCurrentNetwork() extends AtnResult
case class LeaveCursorBehind() extends AtnResult
case class DisableCursor() extends AtnResult

object ArcCondition {
  //incomingEvent, arcToCheck, newRegister, previousRegister, atnMachine
  type FunctionType = (Event, ArcRep, StateRep, StateRep, ATNMachine) => Condition.Result

  def apply(arc: Arc) = new ArcCondition(arc)  
}

class ArcCondition(arc: Arc) {

  import ArcCondition._

  def withCondition(function: FunctionType, moreFunctions: FunctionType*): ArcFunction =
    add(Seq(function) ++ moreFunctions)

  private def add(functions: Seq[FunctionType]): ArcFunction = {
    arc.conditions :::= functions.toList
    new ArcFunction(arc)
  }
}

object ArcFunction {
  //incomingEvent, triggeredArc, newRegister, previousRegister, atnMachine
  type FunctionType = (Event, ArcRep, StateRep, StateRep, ATNMachine) => List[Event]

  def apply(arc: Arc) = new ArcFunction(arc)
}

class ArcFunction(arc: Arc) {
  import ArcFunction._

  def addFunctions(functions: Seq[FunctionType]) {
    add(functions)
  }

  def addFunction(function: FunctionType, moreFunctions: FunctionType*) {
    add(Seq(function) ++ moreFunctions)
  }

  private def add(functions: Seq[FunctionType]): Unit = {
    functions.foreach{f => arc.functions = arc.functions :+ f}
  }
}


case class GeneralStateRep(id: Symbol, register: StateRegister, arcs: List[ArcRep])

case class StateRep(id: Symbol, register: SValSet, arcs: List[ArcRep])

case class ArcRep(name: Symbol)


