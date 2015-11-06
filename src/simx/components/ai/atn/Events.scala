package simx.components.ai.atn

import simx.core.worldinterface.eventhandling.EventDescription
import simx.core.ontology.Symbols


/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 8/23/13
 * Time: 12:40 PM
 */
object Events {
  val reset = new EventDescription(Symbols.reset)
  val resetSubNetwork = new EventDescription(Symbols.iRI)
  val redoSubNetwork = new EventDescription(Symbols.attractorStrength)
}
