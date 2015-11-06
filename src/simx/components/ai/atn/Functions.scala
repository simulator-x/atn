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
import ImplicitConversions._
import simx.core.entity.typeconversion.{TypeInfo, ConvertibleTrait}
import simx.core.ontology.types
import simx.core.worldinterface.eventhandling.Event

/**
 * Created by chrisz and martin on 08/07/15.
 */
object Functions {

  /**
   * Let's a merge only be successful if all paths from split to merge are successfully pursued within the given time frame maximumDeltaInMillis.
   * This is currently the only supported MergeCondition.
   */
  def maxTimeDelta(maximumDeltaInMillis: Long) = new TimeCondition(maximumDeltaInMillis)

  /**
   * Prints information to the current state.
   */
  def printInfo(in: Event, triggeredArc: ArcRep, curReg: StateRep, prevRer: StateRep, atn: ATNMachine): List[Event] = {
    println("----------")
    println("State name\t= '" + curReg.id.name + "'")
    println("TriggeredBy\t= '" + triggeredArc.name.name + "'")
    println("Register\t= '" + (if(curReg.register.isEmpty) "" else curReg.register) + "'")
    println("----------")
    Nil
  }

  /**
   *  Copies all data contained in the triggering event to the current register.
   */
  def copyAllEventDataToRegister(in: Event, triggeredArc: ArcRep, curReg: StateRep, prevRer: StateRep, atn: ATNMachine): List[Event] = {
    in.values.values.flatten.foreach{sVal =>
      curReg.register.add(sVal)
    }
    Nil
  }

  /**
   *  Copies specific data contained in the triggering event to the current register.
   */
  def copyEventDataToRegister(semanticTypesToCopy: ConvertibleTrait[_]*)(in: Event, triggeredArc: ArcRep, curReg: StateRep, prevRer: StateRep, atn: ATNMachine): List[Event] = {
//    println("Copying event data to register")
    in.values.values.flatten.foreach{sVal =>
      if(semanticTypesToCopy.exists(isEqual(sVal.typedSemantics)))
        curReg.register.add(sVal)
    }
    Nil
  }

  /**
   *  Copies the complete content of the previous register to the current register
   */
  def copyRegister(in: Event, triggeredArc: ArcRep, curReg: StateRep, prevReg: StateRep, atn: ATNMachine): List[Event] = {
//    println("Copying previous register data to current register")
    prevReg.register.values.flatten.foreach(value => curReg.register.add(value))
//    println("curReg from " + curReg.id.name + " contains " + curReg.register)
    Nil
  }

  /**
   *  Copies specific data from the previous register to the current register.
   */
  def copyPreviousRegisterValues(semanticTypesToCopy: ConvertibleTrait[_]*)(in: Event, triggeredArc: ArcRep, curReg: StateRep, prevRer: StateRep, atn: ATNMachine): List[Event] = {
    println("Copying previous register data")
    prevRer.register.values.flatten.foreach{ sVal =>
      if(semanticTypesToCopy.exists(isEqual(sVal.typedSemantics))) {
        if(!curReg.register.getAllValuesFor(sVal.typedSemantics.asConvertibleTrait).exists(_ == sVal.value))
          curReg.register.add(sVal)
      }
    }
    Nil
  }

  private def isEqual(c1: TypeInfo[_,_])(c2: ConvertibleTrait[_]) = {
    val res = (c1 >@ c2) && (c2 >@ c1)
    res
  }
}
