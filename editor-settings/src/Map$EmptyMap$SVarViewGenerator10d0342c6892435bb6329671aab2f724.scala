//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import scala.collection.immutable.Map$EmptyMap$

class Map$EmptyMap$SVarViewGenerator10d0342c6892435bb6329671aab2f724 extends SVarViewGenerator[scala.collection.immutable.Map[Any, Any]] {

  def generate: SVarView[scala.collection.immutable.Map[Any, Any]] = new SVarView[scala.collection.immutable.Map[Any, Any]] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the value.
     */
    //Todo: Implement yourself!
    val list = new ListView[String]()
    val component = new ScrollPane(list)

    /**
      *  This function is called whenever the visualized value changes.
      *  It should update component accordingly.
      */
    //Todo: Implement yourself!
    def update(sVarValue: scala.collection.immutable.Map[Any, Any]) {
      list.listData = sVarValue.toList.map(item => {item._1.toString + " -> " + item._2.toString}).sortWith(_ < _).toSeq
    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "Map View"

}