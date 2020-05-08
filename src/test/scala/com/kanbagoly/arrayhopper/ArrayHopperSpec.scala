package com.kanbagoly.arrayhopper

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ArrayHopperSpec extends AnyWordSpecLike with Matchers {

  "array hopper" should {
    "return no indices" when {
      "the array is empty" in {
        ArrayHopper.findHops(Nil) should be(empty)
      }
      "the array only contains [0]" in {
        ArrayHopper.findHops(List(0)) should be(empty)
      }
      "the array has a gap like [1, 0, 1]" in {
        ArrayHopper.findHops(List(1, 0, 1)) should be(empty)
      }
    }
    "return all indices" when {
      "the array contains one element of [1]" in {
        ArrayHopper.findHops(List(1)) should be(List(0))
      }
      "the array contains more element of [1, 1, 1]" in {
        ArrayHopper.findHops(List(1, 1, 1)) should be(List(0, 1, 2))
      }
    }
//    "return indices of [0, 5, 9]" when {
//      "the array contains the given example of [5, 6, 0, 4, 2, 4, 1, 0, 0, 4]" in {
//        ArrayHopper.findHops(List(5, 6, 0, 4, 2, 4, 1, 0, 0, 4)) should be(List(0, 5, 9))
//      }
//    }
  }

}
