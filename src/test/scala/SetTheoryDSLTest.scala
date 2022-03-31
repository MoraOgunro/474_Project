/*
  Mora Ogunro
*/
import SetTheoryDSL.{SetExp, exceptionMap, relationshipMap}
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

  "ExceptionClassDef" should "do it" in {
    ExceptionClassDef("myException").eval
    exceptionMap.contains("myException") shouldBe(true)
  }

  "CatchException" should "do it" in {
    ExceptionClassDef("myException").eval
    CatchException("myException",
      Value(1),
      AbstractClassDef(Value("myClass"),
        field = Field(Value(("f", "public"))),
        constructor = Constructor(Method("initialMethod", NoneCase(), "private"))
      ),
      ThrowException("myException","I want to throw an exception!"),
      Catch(
        Value("In Catch"),
        Variable(Value("storageOfException")),
        Assign(Variable(Value("var")), Variable(Value("Reason")))
      ),
      Value(5)
    ).eval
  }
}

