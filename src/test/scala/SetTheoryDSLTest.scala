import SetTheoryDSL.SetExp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfter
import SetTheoryDSL.SetExp.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class SetTheoryDSLTest extends AnyFlatSpec with Matchers with BeforeAndAfter {
  behavior of "my set theory DSL"
  before {
    Scope("default", ClassDef(Value("myClass"),
      field = Field(Value(("f", "private")), Value(("a", "public")), Value(("b", "private"))),
      constructor = Constructor(Method("initialMethod", Assign(Variable(Value("f")), Value(2)), "private"), Assign(Variable(Value("a")), Value(99), "tiki")))).eval
  }
  "ClassDef" should "Initialize a Class" in {
    val scope = Value(1).getScope("default")
    val myClass = Value(1).getClass(scope, "myClass")
    assert(classOf[mutable.Map[String, Any]].isInstance(myClass))
  }
  "Constructor" should "Define a constructor for a class" in {
    val scope = Value(1).getScope("default")
    val myClass = Value(1).getClass(scope, "myClass")
    assert(myClass.contains("constructor"))
    assert(classOf[Constructor].isInstance(myClass("constructor")))
  }
  "Field" should "Define Fields for a class" in {
    val scope = Value(1).getScope("default")
    val myClass = Value(1).getClass(scope, "myClass")
    assert(myClass.contains("fields"))
    assert(classOf[ArraySeq[SetExp]].isInstance(myClass("fields")))
  }
  "Method" should "Define Methods for a class" in {
    val scope = Value(1).getScope("default")
    val myClass = Value(1).getClass(scope, "myClass")
    val constructor = myClass("constructor").asInstanceOf[Constructor]
    val expressions = constructor.exp
    val method = expressions.head
    assert(classOf[Method].isInstance(method))
  }
  "NewObject" should "Initialize a new object" in {
    val scope = Value(1).getScope("default")
    NewObject("myClass", "newObject").eval
    val myClass = Value(1).getClass(scope, "myClass")
    val myObject = Value(1).getObject(scope, "newObject")
    assert(scope.contains("newObject"))
    assert(myObject.contains("constructor"))
    assert(myObject.contains("private"))
    assert(myObject.contains("public"))
    assert(myObject.contains("protected"))
  }
  "InvokeMethod" should "run the specified method" in {
    val scope = Value(1).getScope("default")
    val myClass = Value(1).getClass(scope, "myClass")
    NewObject("myClass", "newObject").eval
    InvokeMethod("newObject", "initialMethod").eval
    val myObject = Value(1).getObject(scope, "newObject")
    assert(myObject("public").asInstanceOf[mutable.Map[String,Any]]("f") == mutable.HashSet(2))

  }
  "Extends" should "inherit patent class into child class" in {
    val scope = Value(1).getScope("default")
    ClassDef(name = Value("extendedClass"), field = Field(Value(("extendedClassField","private")))) Extends "myClass"
    val extendedClass = Value(1).getClass(scope, "extendedClass")
    assert(classOf[mutable.Map[String, Any]].isInstance(extendedClass))
  }
}

