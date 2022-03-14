/*
  Mora Ogunro
*/
import SetTheoryDSL.{SetExp, relationshipMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfter
import SetTheoryDSL.SetExp.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class SetTheoryDSLTest extends AnyFlatSpec with Matchers with BeforeAndAfter {
  behavior of "my set theory DSL"
  before {
    ClassDef(Value("concreteClass"),
      field = Field(Value(("aField1", "private"))),
      constructor = Constructor(Method("method", Value(1), "private"))).eval

    AbstractClassDef(Value("abstractClass"),
      field = Field(Value(("aField1", "private"))),
      constructor = Constructor(Method("initialMethod", NoneCase(), "private"))).eval

    InterfaceDecl(
        name = Value("interface1"),
        field = Field(Value(("iField1", "public"))),
        constructor = Constructor(Method("iMethod",NoneCase(),"public"))).eval
  }

  "AbstractClassDef" should "have at least one unimplemented method" in {
    AbstractClassDef(Value("myClass"),
      field = Field(Value(("f", "public"))),
      constructor = Constructor(Method("initialMethod", NoneCase(), "private"))
    ).eval

    val createdClass = Value(1).getClass("myClass")
    val result = Value(1).checkAbstractClass(createdClass)
    result shouldBe(true)
  }
  it should "throw an error if there are only concrete methods" in{
    assertThrows[Error] {
      AbstractClassDef(Value("myClass"),
        field = Field(Value(("f", "public"))),
        constructor = Constructor(Method("initialMethod", Assign(Value("f"), Value(1), "public"), "private"))
      ).eval
    }
  }
  "InterfaceDecl" should "have no implemented methods or private fields" in {
    assertThrows[Error] {
      InterfaceDecl(
        name = Value("interface2"),
        field = Field(Value(("iField1", "public"))),
        constructor = Constructor(Method("iMethod",Value(10),"public"))
      ).eval

      val createdInterface = Value(1).getClass("interface2")
      val result = Value(1).checkInterface(createdInterface,"interface2")
      result shouldBe(true)
    }
  }
  "Extends" should "Abstract classes can extend other abstract classes" in {
    AbstractClassDef(Value("myClass"),
      field = Field(Value(("f", "public"))),
      constructor = Constructor(Method("initialMethod", NoneCase(), "private"))
    ) Extends "abstractClass"

    val createdClass = Value(1).getClass("myClass")
    val result = Value(1).checkAbstractClass(createdClass)

    result shouldBe(true)
  }
  it should "Abstract classes can extend concrete classes" in {
    AbstractClassDef(Value("myClass"),
      field = Field(Value(("f", "public"))),
      constructor = Constructor(Method("initialMethod", NoneCase(), "private"))
    ) Extends "concreteClass"

    val createdClass = Value(1).getClass("myClass")
    val result = Value(1).checkAbstractClass(createdClass)

    result shouldBe(true)
  }
  it should "Interfaces can extend other interfaces" in {
    InterfaceDecl(
      name = Value("interface2"),
      field = Field(Value(("iField1", "public"))),
      constructor = Constructor(Method("iMethod",NoneCase(),"public"))
    ) Extends "interface1"

    val createdInterface = Value(1).getClass("interface2")
    val result = Value(1).checkInterface(createdInterface,"interface2")
    result shouldBe(true)
  }
  "Implements" should "Concrete classes should inherit and implement all interface methods" in {
    ClassDef(
      name = Value("myClass"),
      field = Field(),
      constructor = Constructor(Method("iMethod", Value(1),"public"))
    ) Implements "interface1"

    val createdClass = Value(1).getClass("myClass")
    val result = Value(1).checkInterfaceImplementation(createdClass, "myClass")
    result shouldBe(true)
  }
  it should "Concrete classes should throw an error if there are unimplemented methods" in {
   assertThrows[Error]{
     ClassDef(
       name = Value("myClass"),
       field = Field(),
       constructor = Constructor()
     ) Implements "interface1"

   }
  }
  it should "Abstract classes should inherit interface methods" in {
    AbstractClassDef(
      name = Value("myClass"),
      field = Field(),
      constructor = Constructor()
    ) Implements "interface1"

    val createdClass = Value(1).getClass("myClass")
    val result = Value(1).checkInterfaceImplementation(createdClass, "myClass")
    result shouldBe(true)
  }
  it should "Interface cannot implement another interface" in {
    assertThrows[Error] {
      InterfaceDecl(
        name = Value("interface2"),
        field = Field(Value(("iField1", "public"))),
        constructor = Constructor(Method("iMethod",Value(10),"public"))
      ) Implements  "interface1"
    }
  }
  "hasCycle" should "detect cycles of inheritance" in {
    relationshipMap.put("C","B")
    relationshipMap.put("B","A")
    relationshipMap.put("A","C")

    Value(1).hasCycle("A") shouldBe(true)
  }
}

