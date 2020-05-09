package com.kanbagoly.arrayhopper

import java.io.{ByteArrayOutputStream, StringReader}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike


class ArrayHopperSpec extends AnyWordSpecLike with Matchers {

  "array hopper" should {
    "return no indices" when {
      "the array is empty" in {
        findHops() should startWith("failure")
      }
      "the array only contains [0]" in {
        findHops(0) should startWith("failure")
      }
      "the array has an unjumpable gap like [1, 0, 1]" in {
        findHops(1, 0, 1) should startWith("failure")
      }
    }
    "return all indices" when {
      "the array contains one element of [1]" in {
        findHops(1) should startWith("0, out")
      }
      "the array contains more elements of [1, 1, 1]" in {
        findHops(1, 1, 1) should startWith("0, 1, 2, out")
      }
      "the array contains lot of ones of [1, 1, 1, 1, 1]" in {
        findHops(1, 1, 1, 1, 1) should startWith("0, 1, 2, 3, 4, out")
      }
    }
    "return appropriate indices" when {
      "the array has a jumpable gap like [2, 0, 1]" in {
        findHops(2, 0, 1) should startWith("0, 2, out")
      }
      "the array has bigger jumpable gap then the size of the rest of the array [2, 0]" in {
        findHops(2, 0) should startWith("0, out")
      }
      "the array has a better element to choose than to jump a maximum like [2, 3, 1, 1, 1]" in {
        findHops(2, 3, 1, 1, 1) should startWith("0, 1, 4, out")
      }
      "the array contains the given example of [5, 6, 0, 4, 2, 4, 1, 0, 0, 4]" in {
        findHops(5, 6, 0, 4, 2, 4, 1, 0, 0, 4) should startWith("0, 5, 9, out")
      }
    }
  }

  private def findHops(array: Int*): String = {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(new StringReader(array.mkString(System.lineSeparator))) {
        ArrayHopper.main(Array.empty[String])
      }
    }
    out.toString
  }

}
