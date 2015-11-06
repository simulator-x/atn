package simx.components.ai.atn.aspects

import simx.components.ai.atn.gui.GraphContainer
import simx.components.ai.atn.ontology.{types => local}
import simx.core.entity.description.{SValSet, EntityAspect, NamedSValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.{EntityDescription, Symbols}
import simx.core.worldinterface.naming.NameIt


case class GraphAspect(graph: GraphContainer, override val targets: List[Symbol] = Nil) extends EntityAspect(Symbols.atn, Symbols.graph, targets) {

  def getFeatures: Set[ConvertibleTrait[_]] = Set(local.Graph)

  def getProvidings: Set[ConvertibleTrait[_]] = Set(local.Graph)

  def getCreateParams: NamedSValSet =  addCVars(SValSet(local.Graph(graph)))
}

case class GraphDescription(id: String, graph : GraphContainer) extends EntityDescription (

  NameIt(id),
  GraphAspect(graph)
)
