/*
  Mora Ogunro
*/

import SetTheoryDSL.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfter
import SetTheoryDSL.SetExp.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class SetTheoryDSLTest extends AnyFlatSpec with Matchers with BeforeAndAfter {
  behavior of "my set theory DSL"

  "optimizeIF" should "simplify if statements" in {
    optimizeIF(
      IF(condition = 5 > 1,
        thenClause = Value("true"),
        elseClause = Value("false"))
    ) shouldBe Value("true")

    optimizeIF(
      IF(condition = 1 > 100,
        thenClause = Value("true"),
        elseClause = Value("false"))
    ) shouldBe Value("false")
  }
  "optimizeUnion" should "return Value(empty set) if one of the sets is empty" in {

    scopeMap("default").put("set1", mutable.HashSet.empty)
    scopeMap("default").put("set2", mutable.HashSet(1, 2, 3))

    optimizeUnion(
      Union(
        Variable(Value("set1")),
        Variable(Value("set2"))
      )
    ) shouldBe Value(mutable.HashSet.empty)
  }

  "optimizeSetDifference" should "return Value(set) if one of the sets is empty" in {
    scopeMap("default").put("set1", mutable.HashSet.empty)
    scopeMap("default").put("set2", mutable.HashSet(1, 2, 3))

    optimizeSetDifference(
      SetDifference(
        Variable(Value("set1")),
        Variable(Value("set2"))
      )
    ) shouldBe Value(mutable.HashSet(1, 2, 3))
  }

  "optimize" should "run the appropriate optimizations on a set expression" in {
    scopeMap("default").put("set1", mutable.HashSet.empty)
    scopeMap("default").put("set2", mutable.HashSet(1, 2, 3))

    optimize(
      SetDifference(
        Variable(Value("set1")),
        Variable(Value("set2"))
      )
    )
  }

  "Expression" should "optimize and return a partial expression if one of the sets in Union does not exist" in {
    scopeMap("default").put("set2", mutable.HashSet(1, 2, 3))

    Expression(
      Union(
        Variable(Value("set1")),
        Variable(Value("set2"))
      )
    ).eval shouldBe Union(Value(Variable(Value("set1"))), Value(mutable.HashSet(1, 2, 3)))

  }
  it should "optimize and return a partial expression if one of the sets in SetDifference does not exist" in {
    scopeMap("default").put("set2", mutable.HashSet(1, 2, 3))

    Expression(
      SetDifference(
        Variable(Value("set1")),
        Variable(Value("set2"))
      )
    ).eval shouldBe SetDifference(Value(Variable(Value("set1"))), Value(mutable.HashSet(1, 2, 3)))

  }
}

