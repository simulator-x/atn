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

package simx.components.ai.atn

import simx.components.ai.atn.core._
import simx.core.worldinterface.eventhandling.Event

/**
 * Created by martin on 08/07/15.
 */
object ImplicitConversions {

  implicit def toTriple(data: ConditionResult): (Boolean, List[Event], AtnResult) = {
    (data.doTransition, data.eventsToEmit, data.cursorAction)
  }

  /*
    ArcCondition convenience converters
   */

  implicit def toArcCondition5(f: (Event, StateRep, StateRep, ArcRep, ATNMachine) => Condition.Result): ArcCondition.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current, previous, triggeredArc, atn)
    func
  }

  implicit def toArcCondition4(f: (Event, StateRep, StateRep, ArcRep) => Condition.Result): ArcCondition.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current, previous, triggeredArc)
    func
  }

  implicit def toArcCondition3(f: (Event, StateRep, StateRep) => Condition.Result): ArcCondition.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current, previous)
    func
  }

  implicit def toArcCondition2(f: (Event, StateRep) => Condition.Result): ArcCondition.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current)
    func
  }

  implicit def toArcCondition1(f: (Event) => Condition.Result): ArcCondition.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in)
    func
  }

  /*
    ArcFunction convenience converters
 */

  implicit def toArcFunction5(f: (Event, StateRep, StateRep, ArcRep, ATNMachine) => List[Event]): ArcFunction.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current, previous, triggeredArc, atn)
    func
  }
  
  implicit def toArcFunction4(f: (Event, StateRep, StateRep, ArcRep) => List[Event]): ArcFunction.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current, previous, triggeredArc)
    func
  }

  implicit def toArcFunction3(f: (Event, StateRep, StateRep) => List[Event]): ArcFunction.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current, previous)
    func
  }

  implicit def toArcFunction2(f: (Event, StateRep) => List[Event]): ArcFunction.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in, current)
    func
  }

  implicit def toArcFunction1(f: (Event) => List[Event]): ArcFunction.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f(in)
    func
  }

  implicit def toArcFunction0(f: () => List[Event]): ArcFunction.FunctionType = {
    def func(in: Event, triggeredArc: ArcRep, current: StateRep, previous: StateRep, atn: ATNMachine) =
      f()
    func
  }
}
