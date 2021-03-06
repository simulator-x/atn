//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simx.components.mesh.ColoredMesh

class ColoredMeshSVarViewGenerator14d2dd41c75146f2bd4cd100c6ed03c9 extends SVarViewGenerator[ColoredMesh] {

  def generate: SVarView[ColoredMesh] = new SVarView[ColoredMesh] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the value.
     */
    //Todo: Implement yourself!
    val component = new TextArea()

    /**
      *  This function is called whenever the visualized value changes.
      *  It should update component accordingly.
      */
    //Todo: Implement yourself!
    def update(sVarValue: ColoredMesh) {
      val mD = sVarValue
      component.text += "Mesh data:\n"
      component.text += "Indizes Array: "+mD.getIndicesCount+"\n"
      component.text += "Vertices Array: "+mD.getVerticesCount+"\n"
      component.text += "Positions Array: "+mD.positions.length+"\n"
      component.text += "Normals Array: "+mD.normals.length+"\n"
      component.text += "Texture Coords Array: "+mD.texCoords.length+"\n"
      component.text += "Color Array: "+mD.colors.length+"\n"

    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "ColoredMesh View"

}