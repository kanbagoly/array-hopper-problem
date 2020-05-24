package com.kanbagoly.arrayhopper

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

object ArrayHopper {

  def main(args: Array[String]): Unit = {
    val flights = readNumbers().partitionMap(_.toEither) match {
      case (Nil, numbers@_::_) => findHops(numbers)
      case _ => Nil
    }
    println(prepareOutput(flights))
  }

  @tailrec
  private def findHops(numbers: List[Int], acc: List[Int] = Nil, offset: Int = 0): List[Int] = numbers match {
    case 0::_ => Nil
    case x::xs =>
      val horizon = xs take x
      if (horizon.size < x) (offset :: acc).reverse
      else {
        val jump = findMaxIndexPlusValue(horizon)
        findHops(xs drop jump, offset :: acc, offset + jump + 1)
      }
  }

  private def findMaxIndexPlusValue(numbers: List[Int]): Int =
    numbers.zipWithIndex.maxBy { case (v, i) => v + i }._2

  private def readNumbers(): List[Try[Int]] =
    Iterator.continually(StdIn.readLine()).takeWhile(_ != null).map(toNonNegativeInt).toList

  private def toNonNegativeInt(number: String): Try[Int] = Try { requireNonNegative(number.toInt) }

  private def requireNonNegative(number: Int): Int =
    if (number < 0) throw new NumberFormatException("negative number") else number

  private def prepareOutput(numbers: List[Int]): String = numbers match {
    case Nil => "failure"
    case xs => xs.mkString("", ", ", ", out")
  }

}
