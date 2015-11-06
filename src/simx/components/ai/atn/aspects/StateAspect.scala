package simx.components.ai.atn.aspects

import simx.components.ai.atn.ontology.{types => local}
import simx.core.entity.description.{EntityAspect, NamedSValSet, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.{EntityDescription, Symbols}


case class StateDescription(id: Symbol, register: SValSet) extends EntityDescription (

  "ATN_State with ID: " + id.name,
  StateAspect(register)

){
  require(id != null, "the Parameter 'id' must not be null!")
  require(register != null, "the Parameter 'register' must not be null!")
}


case class StateAspect(register: SValSet, override val targets: List[Symbol] = Nil) extends EntityAspect(Symbols.atn, Symbols.state, targets) {


  def getFeatures: Set[ConvertibleTrait[_]] = Set(local.Register)

  def getProvidings: Set[ConvertibleTrait[_]] = Set(local.Register)

  def getCreateParams: NamedSValSet =  addCVars(SValSet(local.Register(register)))
}
