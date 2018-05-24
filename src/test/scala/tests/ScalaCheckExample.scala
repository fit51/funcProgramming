package tests

import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks


class ScalaCheckExample extends PropSpec with PropertyChecks {

  val intList = Gen.listOf(Gen.choose(0, 100))

  property("reverse tests") {
    forAll(intList) { ns =>
      assert(
        ns.reverse.reverse == ns &&
          ns.headOption == ns.reverse.lastOption
      )
    }
  }

  property("failing") {
    forAll(intList) { ns =>
      assert(
        ns.reverse == ns
      )
    }
  }

  property("sum props") {
    forAll(intList) { ns =>
      assert(
        ns.reverse.sum == ns.sum
      )
      assert(
        ns.reverse.sum == ns.sum
      )
    }
  }

}
