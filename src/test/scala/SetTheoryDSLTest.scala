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

  "IF" should "Evaluate an IF statement" in {
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

  "CatchException" should "Evaluate expressions until an exception is discovered." in {
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
  "ThrowException" should "Throw an exception and set the reason within an exception class" in {
    ExceptionClassDef("myException").eval
    ThrowException("myException", "TEST").eval
    exceptionMap.contains("myException") shouldBe(true)
    val exception = exceptionMap("myException")
    exception("Reason") shouldBe("TEST")
  }
  "Catch" should "Catch an exception by running the expressions within itself" in {
    ExceptionClassDef("myException").eval
    exceptionMap.contains("myException") shouldBe(true)
    val exception = exceptionMap("myException")
    ThrowException("myException", "TEST").eval

    Catch(
      Value("In Catch"),
      Variable(Value("storageOfException")),
      Assign(Variable(Value("var")), Variable(Value("Reason")))
    ).eval

    exception.contains("var") shouldBe(true)
    exception("var") shouldBe(mutable.HashSet("TEST"))

  }
}

