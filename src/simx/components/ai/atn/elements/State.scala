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

import java.util.UUID
import simx.components.ai.atn.core._
import simx.core.entity.description.{SValBase, SValSet}
import simx.core.worldinterface.eventhandling.Event

/**
 * Created by chris on 01/08/15.
 */
case class State(id: Symbol) {
  var register  = StateRegister()

  val hiddenID  = UUID.randomUUID()
  var _visited  = false

  def visited = _visited
  def visited_=(value:Boolean):Unit= _visited = value

  private var _isStart  = false
  private var _isEnd    = false
  private var _isSplit  = false
  private var _isMerge  = false


  def asStart() = _isStart = true
  def asEnd()   = _isEnd = true
  def asSplit() = _isSplit = true
  def asMerge() = _isMerge = true

  def isStart = _isStart
  def isEnd   = _isEnd
  def isSplit = _isSplit
  def isMerge = _isMerge

  private var _conditions: List[(Event, ArcRep, StateRep, StateRep, ATNMachine) => Condition.Result] = Nil
  private var _functions = List[ArcFunction.FunctionType]()

  def conditions = _conditions
  def conditions_=(value:List[(Event, ArcRep, StateRep, StateRep, ATNMachine) => Condition.Result]):Unit= _conditions = value

  def functions = _functions
  def functions_=(value:List[ArcFunction.FunctionType]):Unit= _functions = value

  override def toString: String = "[" + id.name + "]"
}

case class StateRegister(private var register: Map[UUID, SValSet]  = Map()){
  def getRegister(cursor: Cursor): SValSet = {
    val oldestParentID = cursor.id.last
    val result: Option[SValSet] = register.get(oldestParentID)
    if(result.isDefined) result.get
    else {
      val newReg = new SValSet()
      register += (oldestParentID -> newReg)
      newReg
    }
  }

  def addToRegister(cursor: Cursor)(sVal: SValBase[Any, _]): Unit ={
    getRegister(cursor).add(sVal)
  }

  def clearRegister(cursor: Cursor): Unit ={
    getRegister(cursor).clear()
  }

  def clearRegister(): Unit = {
    register = Map()
  }
}