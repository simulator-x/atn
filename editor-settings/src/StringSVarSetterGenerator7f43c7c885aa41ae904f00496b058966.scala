//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import java.lang.String

class StringSVarSetterGenerator7f43c7c885aa41ae904f00496b058966 extends SVarSetterGenerator[String] {

  def generate: SVarSetter[String] = new SVarSetter[String] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the SVar setter.
     *  Call
     *  setSvar(newValue: String): Unit
     *  to set new svar value.
     */
    //Todo: Implement yourself!
    val component = new Button("Click to choose File") {
      listenTo(this)
      reactions += {
        case event: event.ButtonClicked =>
          val fc = new FileChooser(new java.io.File("."))
          fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
          fc.multiSelectionEnabled = false
          fc.showDialog(null, "Choose File") match {
            case FileChooser.Result.Approve =>
              setSvar((new java.io.File(".")).toURI.relativize(fc.selectedFile.toURI).getPath)
            case _ => println("NO")
          }
      }
    }

    /**
     * Override update if you want to use the current value of the SVar.
     * This function is initially called once and then every time the value of the SVar changes.
     */
    // override def update(newValue: String) {}

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "File Setter"

}