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

import simx.components.ai.atn.core.Functions
import simx.core.entity.description.SValSet

import scala.collection.mutable

/**
 * Created by chris on 01/08/15.
 */
class Network(_network: mutable.HashMap[State, List[(Arc, State)]]) {

  def this() = this(new mutable.HashMap())

  private var network: mutable.HashMap[State, List[(Arc, State)]] = _network
  
  def get() = network
  def set(net: mutable.HashMap[State, List[(Arc, State)]]): Unit = {
    network = net
  }

  def getStates: Set[State] ={
    var res = network.keySet.toSet
    res = network.values.flatten.map(_._2).toSet ++ res
    res
  }

  def clearRegister(): Unit ={
    getStates.foreach(state => state.register.clearRegister())
  }

  def contains(state: State): Boolean = contains(state.id)

  //TODO could be problematic
  def contains(state: Symbol): Boolean = {
    getStates.map(_.id).contains(state)
  }

  override def toString: String = {

    def getTransitions: String = {
      def getTransition(entry: (State, List[(Arc, State)])):String = {
        var res: String = ""
        entry._2.foreach(transition => res += "[ATN][DEBUG] " + entry._1 + " -" + transition._1 + "-> " + transition._2 + "\n")
        res
      }
      var result: String = ""
      network.foreach(entry =>
        result += getTransition(entry)
      )
      result + "[ATN][DEBUG] ----------------------------------------"
    }
    if(isMainNet) "[ATN][DEBUG]--------------MAIN ATN layout---------------- \n" + getTransitions
    else "[ATN][DEBUG]--------------SUB ATN layout---------------- \n" + getTransitions
  }

  def isMainNet: Boolean = network.exists(_._1.isStart)
  def endStates() = network.keySet.filter{_.isEnd}

}

case class SubNetwork(_network: mutable.HashMap[State, List[(Arc, State)]], startState: State) extends Network(_network) {

  def isStartState(stateID: Symbol) = startState.id == stateID
}
