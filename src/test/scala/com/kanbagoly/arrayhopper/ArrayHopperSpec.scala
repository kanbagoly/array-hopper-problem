package com.kanbagoly.arrayhopper

import java.io.{ByteArrayOutputStream, StringReader}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike


class ArrayHopperSpec extends AnyWordSpecLike with Matchers {

  "array hopper" should {
    "return no indices" when {
      "the array is empty" in {
        val stdin = new StringReader("")
        val stdout = execute(stdin)
        stdout.toString should include("failure")
      }
      "the array only contains [0]" in {
        val stdin = new StringReader("0")
        val stdout = execute(stdin)
        stdout.toString should include("failure")
      }
      "the array has an unjumpable gap like [1, 0, 1]" in {
        ArrayHopper.findHops(List(1, 0, 1)) should be(empty)
      }
    }
    "return all indices" when {
      "the array contains one element of [1]" in {
        ArrayHopper.findHops(List(1)) should be(List(0))
      }
      "the array contains more elements of [1, 1, 1]" in {
        ArrayHopper.findHops(List(1, 1, 1, 1, 1)) should be(List(0, 1, 2, 3, 4))
      }
      "the array contains lot of ones of [1, 1, 1, 1, 1]" in {
        // TODO: Delete
        ArrayHopper.findHops(List(1, 1, 1, 1, 1)) should be(List(0, 1, 2, 3, 4))
      }
    }
    "return appropriate indices" when {
      "the array has a jumpable gap like [2, 0, 1]" in {
        ArrayHopper.findHops(List(2, 0, 1)) should be(List(0, 2))
      }
      "the array has bigger jumpable gap then the size of the rest of the array [2, 0]" in {
        ArrayHopper.findHops(List(2, 0)) should be(List(0))
      }
      "the array has a better element to choose than to jump a maximum like [2, 3, 1, 1, 1]" in {
        ArrayHopper.findHops(List(2, 3, 1, 1, 1)) should be(List(0, 1, 4))
      }
      "the array contains the given example of [5, 6, 0, 4, 2, 4, 1, 0, 0, 4]" in {
        ArrayHopper.findHops(List(5, 6, 0, 4, 2, 4, 1, 0, 0, 4)) should be(List(0, 5, 9))
      }
    }
  }

  private def execute(in: StringReader): ByteArrayOutputStream = {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        ArrayHopper.main(Array.empty[String])
      }
    }
    out
  }
}
