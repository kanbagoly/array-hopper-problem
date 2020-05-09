package com.kanbagoly.arrayhopper

import scala.annotation.tailrec
import scala.io.StdIn

object ArrayHopper {

  def main(args: Array[String]): Unit = {
    val canyon = readNumbers()
    val flights = findHops(canyon)
    println(prepareOutput(flights))
  }

  @tailrec
  def findHops(numbers: List[Int], acc: List[Int] = Nil, offset: Int = 0): List[Int] = numbers match {
    case Nil => Nil
    case 0::_ => Nil
    case x::xs if xs.size < x => (offset :: acc).reverse
    case x::xs =>
      val jump = findMaxIndexPlusValue(xs take x)
      findHops(xs drop jump, offset :: acc, offset + jump + 1)
  }

  private def findMaxIndexPlusValue(numbers: List[Int]): Int =
    numbers.zipWithIndex.maxBy { case (v, i) => v + i }._2

  private def readNumbers(): List[Int] =
    Iterator.continually(StdIn.readLine()).takeWhile(_ != null).map(_.toInt).toList

  private def prepareOutput(numbers: List[Int]): String = numbers match {
    case Nil => "failure"
    case xs => xs.mkString("", ", ", ", out")
  }

}
