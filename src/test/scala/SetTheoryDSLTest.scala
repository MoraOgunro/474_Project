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
    val exceptions = exceptionMap
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
  "IF" should "do it" in {
    IF(true,
      thenClause = Assign(Variable(Value("a")), Value(1)),
      elseClause = Assign(Variable(Value("a")), Value(2))
    ).eval

    val scope = Value(1).getScope("default")
    scope.contains("a") shouldBe(true)
    val new_set = scope("a").asInstanceOf[mutable.HashSet[Any]]
    new_set.contains(1) shouldBe(true)

  }
  "ExceptionClassDef" should "Define an exception" in {
    ExceptionClassDef("myException").eval
    exceptionMap.contains("myException") shouldBe(true)
    val exception = exceptionMap("myException")
    exception.contains("Reason") shouldBe(true)
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
  "ThrowException" should "do it" in {
    ExceptionClassDef("myException").eval
    exceptionMap.contains("myException") shouldBe(true)
  }
  "Catch" should "do it" in {
    ExceptionClassDef("myException").eval
    exceptionMap.contains("myException") shouldBe(true)
  }
}

