package at.fhj.swengb.apps.calculator

import java.net.URL
import java.util.ResourceBundle
import javafx.application.Application
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.control.Label
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object CalculatorApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[CalculatorFX], args: _*)
  }
}

class CalculatorFX extends javafx.application.Application {

  val fxml = "/at/fhj/swengb/apps/calculator/calculator.fxml"
  val css = "/at/fhj/swengb/apps/calculator/calculator.css"

  override def start(stage: Stage): Unit =
    try {
      stage.setTitle("Calculator")
      setSkin(stage, fxml, css)
      stage.show()
      stage.setMinWidth(stage.getWidth)
      stage.setMinHeight(stage.getHeight)
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }

  def setSkin(stage: Stage, fxml: String, css: String): Boolean = {
    val scene = new Scene(mkFxmlLoader(fxml).load[Parent]())
    stage.setScene(scene)
    stage.getScene.getStylesheets.clear()
    stage.getScene.getStylesheets.add(css)
  }

  def mkFxmlLoader(fxml: String): FXMLLoader = {
    new FXMLLoader(getClass.getResource(fxml))
  }

}

class CalculatorFxController extends Initializable {

  val calculatorProperty: ObjectProperty[RpnCalculator] = new SimpleObjectProperty[RpnCalculator](RpnCalculator())
  @FXML var Label1: Label = _
  @FXML var Label2: Label = _
  @FXML var Label3: Label = _
  var clearall: Boolean = false

  override def initialize(location: URL, resources: ResourceBundle) = {

  }

  def insertThis(str: String): Unit = {
    str match {
      case "Addition" => getCalculator().push(Op("+")) match {
        case Success(c) => setCalculator(c)
          Label2.setText(getCalculator().stack.last match { case v: Val => v.value.toString })
          Label1.setText("")
      }
      case "Subtraction" => getCalculator().push(Op("-")) match {
        case Success(c) => setCalculator(c)
          Label2.setText(getCalculator().stack.last match { case v: Val => v.value.toString })
          Label1.setText("")
      }
      case "Multiplication" => getCalculator().push(Op("*")) match {
        case Success(c) => setCalculator(c)
          Label2.setText(getCalculator().stack.last match { case v: Val => v.value.toString })
          Label1.setText("")
      }
      case "Divison" => getCalculator().push(Op("/")) match {
        case Success(c) => setCalculator(c)
          Label2.setText(getCalculator().stack.last match { case v: Val => v.value.toString })
          Label1.setText("")
      }
      case "comma" => if (Label3.getText.isEmpty) insertThis("0.") else if (Label3.getText.count(_ == '.') == 0) insertThis(".")

      case "enter" => {
        if (!Label3.getText.isEmpty) {
          sgn()
          if (true)
            if (Label2.getText.isEmpty) {
              Label2.setText(Label3.getText)
              Label3.setText("")
            }
            else {
              Label1.setText(Label2.getText)
              Label2.setText(Label3.getText)
              Label3.setText("")
            }
        }
        else {
          insertThis("0")
        }
      }
      case "clear" => {
        if (clearall) {
          setCalculator(RpnCalculator())
          Label1.setText("")
          Label2.setText("")
          Label3.setText("")
          clearall = false
        }
        else {
          Label3.setText("0")
          clearall = true
        }
      }
      case "Change" => {
        if (!Label3.getText.isEmpty) {
          if (Label3.getText.head.equals('-'))
            Label3.setText(Label3.getText.tail)
          else
            Label3.setText('-' + Label3.getText)
        }
        else Label3.setText("-")
      }
      case _ =>
        if (Label3.getText.isEmpty) Label3.setText(str)
        else Label3.setText(Label3.getText + str)
    }
  }

  def sgn(): Unit = {
    getCalculator().push(Op(Label3.getText)) match {
      case Success(c) => setCalculator(c)
      case Failure(e) => // show warning / error
    }
    getCalculator().stack foreach println
  }

  def getCalculator(): RpnCalculator = calculatorProperty.get()

  def setCalculator(rpnCalculator: RpnCalculator): Unit = calculatorProperty.set(rpnCalculator)

  @FXML private def clickit(click: ActionEvent): Unit = {
    val btn: String = click.getSource.asInstanceOf[javafx.scene.control.Button].toString
    btn match {
      case "Button[id=button1, styleClass=button]'1'" => insertThis("1")
      case "Button[id=button2, styleClass=button]'2'" => insertThis("2")
      case "Button[id=button3, styleClass=button]'3'" => insertThis("3")
      case "Button[id=button4, styleClass=button]'4'" => insertThis("4")
      case "Button[id=button5, styleClass=button]'5'" => insertThis("5")
      case "Button[id=button6, styleClass=button]'6'" => insertThis("6")
      case "Button[id=button7, styleClass=button]'7'" => insertThis("7")
      case "Button[id=button8, styleClass=button]'8'" => insertThis("8")
      case "Button[id=button9, styleClass=button]'9'" => insertThis("9")
      case "Button[id=buttonC, styleClass=button]'C'" => insertThis("clear")
      case "Button[id=buttonDiv, styleClass=button]'/'" => insertThis("Divison")
      case "Button[id=buttonMul, styleClass=button]'x'" => insertThis("Multiplication")
      case "Button[id=buttonMin, styleClass=button]'-'" => insertThis("Subtraction")
      case "Button[id=buttonPlu, styleClass=button]'+'" => insertThis("Addition")
      case "Button[id=buttonKomma, styleClass=button]','" => insertThis("comma")
      case "Button[id=buttonEnter, styleClass=button]'Enter'" => insertThis("enter")
      case "Button[id=button0, styleClass=button]'0'" => insertThis("0")
      case "Button[id=buttonPluMin, styleClass=button]'+/-'" => insertThis("Change")
      case _ => //some crazy shit ;-)
    }
    println(btn)
  }


}