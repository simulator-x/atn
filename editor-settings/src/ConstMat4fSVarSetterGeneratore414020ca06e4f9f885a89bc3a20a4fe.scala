/*
 * Copyright 2013 The SIRIS Project
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

//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simplex3d.math.floatx._

class ConstMat4fSVarSetterGeneratore414020ca06e4f9f885a89bc3a20a4fe extends SVarSetterGenerator[ConstMat4f] {

  def generate: SVarSetter[ConstMat4f] = new SVarSetter[ConstMat4f] {

//AutoGenerated END
//Put your code below

    var initialized = false

       override def update(newValue: ConstMat4f): Unit = {
         if(!initialized) {

           scaleX.text = functions.length(Vec3f(newValue.m00, newValue.m10, newValue.m20)).toString
           scaleY.text = functions.length(Vec3f(newValue.m01, newValue.m11, newValue.m21)).toString
           scaleZ.text = functions.length(Vec3f(newValue.m02, newValue.m12, newValue.m22)).toString
           scaleProp.text = "1.0"

           posX.text = newValue(3).x.toString
           posY.text = newValue(3).y.toString
           posZ.text = newValue(3).z.toString

           var xAngle = 0f
           var yAngle = 0f
           var zAngle = 0f

           val m: ConstMat4f = (newValue)

           if((1f - scala.math.abs(m.m20)) >= 0.0001f) {
             yAngle = -1.0f * scala.math.asin(m.m20).toFloat
             xAngle = scala.math.atan2(m.m21/scala.math.cos(yAngle), m.m22/scala.math.cos(yAngle)).toFloat
             zAngle = scala.math.atan2(m.m10/scala.math.cos(yAngle), m.m00/scala.math.cos(yAngle)).toFloat
           }
           else {
             zAngle = 0
             if(m.m20 < 0) {
               yAngle = math.Pi.toFloat / 2f
               xAngle = scala.math.atan2(m.m01, m.m02).toFloat
             }
             else {
               yAngle = math.Pi.toFloat / -2f
               xAngle = scala.math.atan2(-1f * m.m01, -1f * m.m02).toFloat
             }
           }

//        println(if(degrees(xAngle).toInt >= 0) degrees(xAngle).toInt else (degrees(xAngle).toInt + 360))
//        println(if(degrees(yAngle).toInt >= 0) degrees(yAngle).toInt else (degrees(yAngle).toInt + 360))
//        println(if(degrees(zAngle).toInt >= 0) degrees(zAngle).toInt else (degrees(zAngle).toInt + 360))

           x.value = if(functions.degrees(xAngle).toInt >= 0) functions.degrees(xAngle).toInt else (functions.degrees(xAngle).toInt + 360)
           y.value = if(functions.degrees(yAngle).toInt >= 0) functions.degrees(yAngle).toInt else (functions.degrees(yAngle).toInt + 360)
           z.value = if(functions.degrees(zAngle).toInt >= 0) functions.degrees(zAngle).toInt else (functions.degrees(zAngle).toInt + 360)

           posX.enabled = true
           posY.enabled = true
           posZ.enabled = true
           x.enabled = true
           y.enabled = true
           z.enabled = true
         /*  scaleX.enabled = true
           scaleY.enabled = true
           scaleZ.enabled = true*/
           scaleProp.enabled = true
           initialized = true
         }
       }

       val x = new Slider {
         max = 360
         min = 0
         paintTicks = true
         paintTrack = true
         paintLabels = true
         majorTickSpacing = 90
         enabled = false
//      maximumSize = new Dimension(300, 50)
       }

       val xWithLabel = new BoxPanel(Orientation.Horizontal) {
         contents += new Label("Rotate X")
         contents += x
       }

       val y = new Slider {
         max = 360
         min = 0
         paintTicks = true
         paintTrack = true
         paintLabels = true
         majorTickSpacing = 90
         enabled = false
//      maximumSize = new Dimension(300, 50)
       }

       val yWithLabel = new BoxPanel(Orientation.Horizontal) {
         contents += new Label("Rotate Y")
         contents += y
       }

       val z = new Slider {
         max = 360
         min = 0
         paintTicks = true
         paintTrack = true
         paintLabels = true
         majorTickSpacing = 90
         enabled = false
//      maximumSize = new Dimension(300, 50)
       }

       val zWithLabel = new BoxPanel(Orientation.Horizontal) {
         contents += new Label("Rotate Z")
         contents += z
       }

       val posX = new TextField(){enabled = false; maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
       val posY = new TextField(){enabled = false; maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
       val posZ = new TextField(){enabled = false; maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}

       val posWithLabel = new GridPanel(1,6) {
         contents += new Label("X"){maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
         contents += posX
         contents += new Label("Y"){maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
         contents += posY
         contents += new Label("Z"){maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
         contents += posZ
       }

       val scaleX = new TextField(){enabled = false; maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
       val scaleY = new TextField(){enabled = false; maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
       val scaleZ = new TextField(){enabled = false; maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
       val scaleProp = new TextField(){enabled = false; maximumSize = new Dimension(100, 30); preferredSize = new Dimension(100, 30); minimumSize = new Dimension(100, 30)}

       val scaleWithLabel = new GridPanel(1, 1) {
//         contents += new GridPanel(1, 6) {
//           contents += new Label("Scale X") {maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
//           contents += scaleX
//           contents += new Label("Scale Y") {maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
//           contents += scaleY
//           contents += new Label("Scale Z") {maximumSize = new Dimension(30, 30); preferredSize = new Dimension(30, 30); minimumSize = new Dimension(30, 30)}
//           contents += scaleZ
//         }
         contents += new BoxPanel(Orientation.Horizontal) {
           contents += new Label("Scale proportional")/* {maximumSize = new Dimension(50, 50); preferredSize = new Dimension(50, 50); minimumSize = new Dimension(50, 50)}*/
           contents += scaleProp
           /*contents += new Slider {

             max = 10
             min = 0.1f
             paintTicks = true
             paintTrack = true
             paintLabels = true
             majorTickSpacing = 90
             enabled = true
             //      maximumSize = new Dimension(300, 50)
           }  */
         }
       }

       /**
        *  The scala.swing.Component that visualizes the SVar setter.
        *  Call
        *  setSvar(newValue: ConstMat4f): Unit
        *  to set new svar value.
        */
       //Todo: Implement yourself!
       val component = new BoxPanel(Orientation.Vertical) {
         contents += xWithLabel
         contents += yWithLabel
         contents += zWithLabel
         contents += posWithLabel
         contents += scaleWithLabel

         listenTo(x, y, z, posX, posY, posZ, scaleX, scaleY, scaleZ, scaleProp)

         reactions += {
           case event.ValueChanged(src) if((src == x) || (src == y) || (src == z) ) => setTheSvar
           case event.EditDone(src) if((src == posX) || (src == posY) || (src == posZ) || (src == scaleProp)) => setTheSvar
         }
       }

       def setTheSvar = {
         try {
           if(initialized)
             setSvar(ConstMat4f(Mat4x3f.scale(scaleProp.text.toFloat).rotateX(functions.radians(x.value)).rotateY(functions.radians(y.value)).rotateZ(functions.radians(z.value)).translate(Vec3f(posX.text.toFloat, posY.text.toFloat, posZ.text.toFloat))))
         } catch {
           case e : Throwable => println(e)
         }
       }

     }


  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "Transformation Setter"

}