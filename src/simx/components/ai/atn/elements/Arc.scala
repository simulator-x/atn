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

import simx.components.ai.atn.core._
import simx.core.worldinterface.eventhandling.Event

/**
 * Created by chris on 01/08/15.
 */
case class Arc(id: Symbol) {
  var triggered = false
  var isSubArc = false
  var isHiddenSplit = false
  var isHiddenMerge = false
  var conditions: List[(Event, ArcRep, StateRep, StateRep, ATNMachine) => Condition.Result] = Nil
  var functions = List[ArcFunction.FunctionType]()
  var isEpsilonArc = false

  var arcType: ArcType = Normal()

  override def toString: String = "-" + id.name + "-"
}

abstract class ArcType
case class Sub() extends ArcType
case class Normal() extends ArcType
case class Epsilon() extends ArcType
