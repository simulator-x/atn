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
import scala.collection.mutable

/**
 * Created by chris on 01/08/15.
 */
case class Cursor(var state: State, var pushDownStore: mutable.Stack[State] = new mutable.Stack[State](), id: List[UUID] = UUID.randomUUID() :: Nil) {
  var rootState = state
  var toDelete = false
  var enabled = true
  var tempStore: mutable.Stack[State] = new mutable.Stack[State]()

  def delete(){
    toDelete = true
  }

  override def toString: String = {
    "[" + state.id.name + "] with " + pushDownStore.map(_.id.name)
  }

  def ready(): Unit ={
    tempStore.reverse.foreach(state => pushDownStore.push(state))
    //pushDownStore = tempStore.retempStore.clone().a
    tempStore.clear()
  }
  /**
   * replicates this cursor (different ID)
   * id is used if a cursors has to to be relocated for a subArc
   * a new cursor with same id will be made and if a normal or epsilon transition is made the original cursor
   * will be found by the same hidden id as the new cursor and deleted
   * @return a replica of this cursor with a different id
   */
  def replicate: Cursor = {
    var ids = id
    ids ::= UUID.randomUUID()
    val newCursor = new Cursor(state, pushDownStore.clone(), ids)
    newCursor.rootState = rootState
    newCursor.tempStore = tempStore.clone()
    newCursor
  }

  /**
   * copies the cursor (same id)
   * @return an exact copy with the same id
   */
  def copy: Cursor = {
    val newCursor = new Cursor(state, pushDownStore.clone(), id)
    newCursor.rootState = rootState
    newCursor.tempStore = tempStore.clone()
    newCursor
  }

  def moveTo(targetState: State) {
    state = targetState
    rootState = state
  }

  override def hashCode(): Int = {
    state.hashCode() + pushDownStore.hashCode() + rootState.hashCode()
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case anObj: Cursor =>
        anObj.state.equals(state) &&
        anObj.pushDownStore == pushDownStore &&
        anObj.rootState == rootState
      case _ => false
    }
  }
}
