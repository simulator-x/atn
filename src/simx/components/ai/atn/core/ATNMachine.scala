package simx.components.ai.atn.core

import simx.components.ai.atn.Events
import simx.components.ai.atn.aspects.{GraphDescription, StateDescription}
import simx.components.ai.atn.elements._
import simx.components.ai.atn.gui.Control
import simx.components.ai.atn.ontology.{types => local}
import simx.core.component.Component
import simx.core.entity.Entity
import simx.core.entity.component.{ComponentAspect, EntityCreationHandling}
import simx.core.entity.description.{EntityAspect, NamedSValSet, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.helper.{AllOrNothing, AutomaticLocalEntityRepresentation, Nothing}
import simx.core.ontology.{EntitySValDescription, Symbols, types}
import simx.core.worldinterface.eventhandling.{Event, EventDescription, EventHandler, EventProvider}

import scala.collection.mutable




case class ATNMachineAspect(
  componentName: Symbol = 'UnnamedATNMachine,
  network: AugmentedTransitionNetwork,
  autoResetAfter: Option[Long] = None,
  drawGraphs: Boolean = false,
  newConfig: Boolean = false,
  entityTypesForLocalRepresentation: AllOrNothing[List[EntitySValDescription]] = Nothing
  ) extends ComponentAspect[ATNMachine](Symbols.atn, componentName, Seq(network, autoResetAfter, drawGraphs, newConfig, entityTypesForLocalRepresentation))
{
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set()
  def getCreateParams: NamedSValSet = NamedSValSet(aspectType)
}

/**
 *
 * @param componentName The component's name
 * @param atn The augmented transition atNetwork
 */

class ATNMachine (
    override val componentName: Symbol,
    atn: AugmentedTransitionNetwork,
    autoResetAfter: Option[Long],
    drawGraphs: Boolean,
    newConfig: Boolean,
    val entityTypesForLocalRepresentation: AllOrNothing[List[EntitySValDescription]]
  ) extends Component(componentName, Symbols.atn) with EventProvider with EventHandler with EntityCreationHandling with Functions with AutomaticLocalEntityRepresentation {


  // extracting atn layout and setting
  val atnLayout = atn.getLayout
  private var startState = State('____void)
  private val network = atnLayout.network.get()
  private val subNetworks = atnLayout.subNetworks
  private val mainNetwork = atnLayout.mainNetwork.get

  private var nextResetTime = Long.MaxValue

  private var currentCursors: List[Cursor] = List()
  private var newCursors: List[Cursor] = List()
  private var input: Option[Event] = None

  private var graphContainerEntities: mutable.HashMap[String, Entity] = mutable.HashMap()
  private val frontEndRepresentation: Map[Symbol, GeneralStateRep] = frontEndRep(network)


  private val control = new Control(atnLayout)
  if(drawGraphs){control.displayGUI()}


  /**
   * called when the actor is started
   */
  override protected def startUp() {
    registerEvents()
    addJobIn(10000){createEntities()}
  }



  iniATNMachine()




  /**
   *  ----------ATN-Main functionality ----------
   *
   */



  override protected def handleEvent(e: Event) {

    input = Some(e)

    e.name match {
      case Events.reset.name =>
        if(e.values.firstValueFor(types.Identifier) == componentName){
          iniATNMachine()
        }
      case Events.redoSubNetwork.name =>
        val stateID = e.values.firstValueFor(types.Identifier)
        subNetworks.foreach(net => if(net.contains(stateID)) {
          println("[ATN][CORE] Redoing sub ATN")
          val netBegins = atnLayout.states(net.startState.id)
          netBegins.foreach{begin =>
            if(!currentCursors.exists(_.state.id == begin.id)){
              val newCursor = new Cursor(begin)
              currentCursors ::= newCursor
            }
          }
          resetSubNetwork(net)
        })
      case Events.resetSubNetwork.name =>
        val stateID = e.values.firstValueFor(types.Identifier)
        subNetworks.foreach(net => if(net.contains(stateID)) {
          resetSubNetwork(net)
        })
        println(currentCursors)
      case _ =>
        if(Atn.debug){
          println()
          println("[ATN][DEBUG]---------------ROUND STARTED------------------")
        }
        processCursorsForEpsilon()
        processCursors()
        processCursorsForEpsilon()

        addCursorToStartState()

        if(Atn.RESET_ON_ENDSTATE) resetOnEndState()


        updateGui(currentCursors)
        updateEditor(currentCursors)

        if(Atn.debug) {
          println("[ATN][DEBUG] Current Cursors: " + currentCursors)
          println("[ATN][DEBUG]---------------ROUND FINISHED------------------")
          println()
        }
      }
    }






  /**
   * --------------- ATN-Traversing functionality ------------------
   *
   */


  /**
   * sets epsilonRound = false and checks all normal and sub arcs of all cursors
   */
  def processCursors(): Unit ={
    epsilonRound = false
    currentCursors.foreach{cursor =>
      val (didTrans, newCur) = checkTransitions(cursor)
    }
    currentCursors = unify(currentCursors, newCursors)
  }

  /**
   * processes epsilon and sub arcs of all cursors
   */
  private var epsilonRound = false
  def processCursorsForEpsilon(): Unit ={
    var processedEpsilon = false
    currentCursors.foreach{cursor =>
      val(didTrans, newCur) = checkForEpsilon(cursor)
      if(didTrans) {
        processedEpsilon = true
      }
    }
    currentCursors = unify(currentCursors, newCursors)
    if(processedEpsilon) processCursorsForEpsilon()
  }

  private def checkForEpsilon(cursor: Cursor): (Boolean, Option[Cursor]) ={
    epsilonRound = true
    checkTransitions(cursor)
  }

  /**
   *  checks all transitions of a given cursor
   * @param cursor the cursors which transitions are checked
   * @return if the cursors made a transition
   */
  private def checkTransitions(cursor: Cursor): (Boolean, Option[Cursor]) = {
    var didTransition = false
    var cur: Option[Cursor] = None
    if(cursor.enabled){
      val state = cursor.state
      atnLayout.getTransitions(state).foreach{trans =>
        val arc = trans._1
        val targetState = trans._2
        arc.arcType match {
          case _type: Epsilon =>
            val (x, y) = processArc(cursor, arc, targetState)
            didTransition = x
            cur = y
          case _type: Sub =>
            val (x, y) = processSub(cursor, arc, targetState)
            didTransition = x
            cur = y
          case _ => if(!epsilonRound) {
            val (x, y) = processArc(cursor, arc, targetState)
            didTransition = x
            cur = y
          }
        }
      }
//      if(didTransition) {
//        currentCursors = currentCursors.filterNot(_.id == cursor.id)
//      }
    }
    (didTransition, cur)
  }



  /**
   * unifes the currentCursors which have not yet been moved this turn with the newCursors
   * @param currentCur cursors which have not yet been moved
   * @param newCur cursors which have been moved
   * @return
   */
  private def unify(currentCur: List[Cursor], newCur: List[Cursor]): List[Cursor] = {
    var result: List[Cursor] = newCur ::: currentCur
    val hashSet: mutable.HashSet[Cursor] = mutable.HashSet()
    result.foreach(c => hashSet.add(c))
    result = hashSet.toList
    newCursors = Nil
    result
  }

  def deleteCursor(state: Symbol): Unit ={
    currentCursors = currentCursors.filterNot(cur => cur.state.id == state)
    newCursors = newCursors.filterNot(cur => cur.state.id == state)
  }

  /**
   * creates a copy of the given cursor (cursor with same id) and moves it to the start state of the lower network
   * checks the transition for the cursor copy in the lower network
   * cursor copy with same id is important due to the moveCursor function
   * @param cursor the cursor that triggers the arc
   * @param arc the triggered sub arc
   * @param nextState the state the arc points to (in its own hierarchy)
   * @return
   */
  private def processSub(cursor: Cursor, arc: Arc, nextState: State): (Boolean, Option[Cursor]) = {
    val cursorCopy = cursor.copy
    val newState = network.find(entry => entry._1.id == arc.id).get._1
    cursorCopy.state = newState
    cursorCopy.tempStore.push(nextState)
    val (didTransition, cur) = checkTransitions(cursorCopy)
    if(didTransition) {
      (didTransition, cur)
    } else (false, Some(cursor))
  }

  /**
   * checks the conditions of an arc, triggers its functionality and moves the cursorcursorCopy
   * the only function that can trigger a transition (epsilon or normal)
   * @param cursor the cursor that triggers the arc
   * @param arc the triggered arc
   * @param targetState the state the arc points to
   * @return if the arc was traversed (all its conditions have to be true)
   */
  private def processArc(cursor: Cursor,
                         arc: Arc,
                         targetState: State
                          ): (Boolean, Option[Cursor]) = {
      if(checkConditions(cursor, arc, targetState)){
        if(Atn.debug) {
          val register = getRegisters(cursor)(cursor.rootState, targetState)
          println("[ATN][DEBUG] Triggered arc: " + arc)
          println("[ATN][DEBUG] Between " + register._1.id.name +" and "+ register._2.id.name)
        }
        executeFunctions(cursor)(cursor.rootState, arc, targetState)
        updateAutoResetTime()
        val newCursor = moveCursor(cursor, targetState)
        checkForEndState(newCursor)
        (true, Some(newCursor))
      } else (false, None)
  }

  /**
   * checks if the given cursor resides on an endState
   * if he does and the cursors history is not empty it moves the cursors to the first state in its history
   * basically from the end of a sub atn to the upper atn
   * recursive if multiple hierarchies have been traversed up
   * @param cursor the cursors that is checked
   */
  private def checkForEndState(cursor: Cursor): Unit ={
    val oldState = cursor.state
    if(cursor.state.isEnd && cursor.pushDownStore.nonEmpty){
      val newState = cursor.pushDownStore.pop()
      cursor.moveTo(newState)
      oldState.register.getRegister(cursor).values.flatten.foreach(newState.register.addToRegister(cursor))
      oldState.register.clearRegister(cursor)
      checkForEndState(cursor)
    }
  }

  /**
   * checks all conditions of a given arc and only returns true if all conditions are true
   * Note: events and atnResults from the conditions are stored and executed after all conditions been checked
   * @param cursor the cursor which triggered the arc
   * @param arc the arc which conditions will be checked
   * @param targetState the state the arc points to
   * @return if(all conditions == true) return true else false
   */
  private def checkConditions(cursor: Cursor, arc: Arc, targetState: State): Boolean = {
    val state = cursor.rootState
    val registers = getRegisters(cursor)(state, targetState)
    var result = true
    var eventList: List[Event] = Nil
    var atnResults: List[AtnResult] = Nil
    arc.conditions.foreach{condition =>
      val (res, events, atnResult) = condition(input.get, new ArcRep(arc.id), registers._2, registers._1, this)
      eventList :::= events
      atnResults ::= atnResult
      if (!res) result = false
    }
    atnResults.foreach {
      case r: ResetCurrentNetwork =>
        eventList ::= Events.resetSubNetwork.createEvent(types.Identifier(registers._1.id))
      case r: DisableCursor => cursor.enabled = false
      case r: LeaveCursorBehind =>
        val newCur = cursor.replicate
        newCur.tempStore = new mutable.Stack[State]()
        newCur.state = cursor.rootState
        newCursors ::= newCur
      case _ =>
    }
    processEvents(eventList)
    result
  }

  /**
   * executes the functions of a given arc.
   * for that it needs the previous and target states registers
   * @param state the state the arc leaves
   * @param arc the arc which functions will be executed
   * @param targetState the state the arc points to
   */
  private def executeFunctions(cursor: Cursor)(state: State, arc: Arc, targetState: State): Unit ={
    val (prevReg, curReg) = getRegisters(cursor)(state, targetState)
    var eventList: List[Event] = Nil
    arc.functions.foreach(function => {
      eventList :::= function(input.get, new ArcRep(arc.id), curReg, prevReg, this)
    })
    processEvents(eventList)
  }


  /**
   * ------------------------ ATN-Helper functionality ---------------------------------------
   *
   */


  /**
   * gets the original cursor from currentCursors in case subArcs created copies of that cursor
   * deletes the cursor from the currentCursors and adds them to the newCursors
   * @param oldCursor could be the modified copy of the original cursor if subArc
   * @param newState target state
   */
  private def moveCursor(oldCursor: Cursor, newState: State): Cursor = {
    //val cursorToMove = currentCursors.filter(_.id == oldCursor.id).head
    currentCursors = currentCursors.filterNot(_.id == oldCursor.id)
    val newCursor = oldCursor.replicate
    newCursor.moveTo(newState)
    newCursors ::= newCursor
    newCursor.ready()
    newCursor
  }

  private def resetRegister(state : State){
    state.register.clearRegister()
  }

  private def getRegisters(cursor: Cursor)(startState: State, targetState: State): (StateRep, StateRep) = {
    val startStateRep = frontEndRepresentation(startState.id)
    val targetStateRep = frontEndRepresentation(targetState.id)
    (StateRep(startStateRep.id, startStateRep.register.getRegister(cursor), startStateRep.arcs),
      StateRep(targetStateRep.id, targetStateRep.register.getRegister(cursor), targetStateRep.arcs))
  }

  private def updateAutoResetTime() {
    autoResetAfter.collect{ case autoRestTime =>
      nextResetTime = System.currentTimeMillis() + autoRestTime
    }
  }

  /**
   * registers for atns defined in the augmentedTransitionNetwork and the atn-machine system events
   */
  private def registerEvents(): Unit ={
    val inputTypes = Events.reset :: Events.redoSubNetwork :: Events.resetSubNetwork :: atn.inputTypes
    val outputTypes = Events.reset :: Events.redoSubNetwork :: Events.resetSubNetwork :: atn.outputTypes
    inputTypes.foreach(requestEvent)
    outputTypes.foreach(provideEvent(_, None))
  }

  private def processEvents(list : List[Event]){
    if (list.nonEmpty) {
      list.foreach(emitEvent)
    }
  }


  /**
   * resets the atn-machine if Events.reset is received
   */
  private def iniATNMachine(){
    println("[ATN][CORE] Resetting ATNMachine")
    startState = network.keySet.find(state => state.isStart).get
    currentCursors = Nil
    currentCursors = new Cursor(startState) :: currentCursors
    updateGui(currentCursors)
    updateEditor(currentCursors)
    nextResetTime = Long.MaxValue
    newCursors = Nil
    network.keySet.foreach(resetRegister)
    subNetworks.foreach(net => net.getStates.foreach(resetRegister))
  }

  def resetSubNetwork(net: Network): Unit ={
    println("[ATN][CORE] Resetting sub ATN")
    net.clearRegister()
    currentCursors = currentCursors.filterNot(cursor => net.contains(cursor.state))
    newCursors = newCursors.filterNot(cursor => net.contains(cursor.state))
    updateGui(currentCursors)
    updateEditor(currentCursors)
  }

  private def updateGui(cursors : List[Cursor]){
    control.updateGui(cursors)
  }

  private def updateEditor(cursors : List[Cursor]){
    control.updatedGraphContainer(cursors)
    graphContainerEntities.foreach(gC => {
      gC._2.getSVars(local.Graph).head._2.set(control.graphContainer.get(gC._1).get)
    })
  }


  //TODO: correctly displaying new registers in entities
  /**
   * Creates State entities in order to inspect registers via Simx-Editor
   */
  private def createEntities(): Unit = {
    def inLocalEntityRep(e : Entity, id : String){
      graphContainerEntities += (id -> e)
    }
    println("[ATN][CORE]Creating entities ...")
    control.graphContainer.foreach(gC => GraphDescription(id = "Graph: " + gC._1,
      graph = gC._2).realize(e => inLocalEntityRep(e, gC._1)))
    network.keySet.foreach(state => StateDescription(id = state.id, register = new SValSet()).realize(e => ()))
  }

  private def addCursorToStartState(): Unit ={
    val cursorsOnStart = currentCursors.filter(_.state.id == startState.id)
    if(cursorsOnStart.size > 1) {
      currentCursors = currentCursors.filterNot(cursorsOnStart.contains(_))
    }
     if(!currentCursors.exists(_.state == startState)) {
         currentCursors ::= new Cursor(startState)
      }
  }

  private def resetOnEndState(): Unit ={
    currentCursors.foreach{cur =>
      if(mainNetwork.endStates().map(_.id).contains(cur.state.id)){
        if(Atn.debug) println("[ATN][Debug] Reset on Endstate")
        iniATNMachine()
      }}
  }

  /**
   * ----------------------- SimulatorX-Component functionality ------------------------------
   *
   */

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) =
    SValSet()

  protected def finalizeConfiguration(e: Entity){}

  /**
   * Called for each simulation step the component should execute. The freqency with which this method is called
   * depends on the used [[simx.core.component.ExecutionStrategy]].
   */
  protected def performSimulationStep() {
    autoResetAfter.collect{ case resetTime =>
      if(System.currentTimeMillis() > nextResetTime) {
        iniATNMachine()
      }
    }
    simulationCompleted()
  }

  /**
   * provideInitialValues has to be called within this method with the full set of initial values to be provided
   * @note the component should create its local representation within this mehtod
   * @param toProvide the convertibletraits for which values shall be provided
   * @param aspect the aspect providing the context for this method call
   * @param e the entity to be filled
   * @param given a set of create parameters that were already provided
   *
   */
  protected def requestInitialValues(
                                      toProvide: Set[ConvertibleTrait[_]],
                                      aspect: EntityAspect,
                                      e: Entity,
                                      given: SValSet) {
    provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
  }

  /**
   * method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  protected def removeFromLocalRep(e: Entity) {}

  /**
   * used to integrate the entity into the simulation
   * @param e the entity to be integrated
   * @param aspect the aspect which the component has to process
   */
  protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {}

  def localEntityData(entName: String) = getLocalEntityData(entName)

  /**
   * (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params: SValSet) {}
}

case class ConditionResult(doTransition: Boolean, eventsToEmit: List[Event] = Nil, cursorAction: AtnResult = KeepCursor())