package com.kanbagoly.arrayhopper

import scala.annotation.tailrec
import scala.io.StdIn

object ArrayHopper {

  def main(args: Array[String]): Unit = {
    val flights = readNumbers().partitionMap(identity) match {
      case (Nil, numbers) => findHops(numbers)
      case (_, _) => Nil
    }
    println(prepareOutput(flights))
  }

  @tailrec
  private def findHops(numbers: List[Int], acc: List[Int] = Nil, offset: Int = 0): List[Int] = numbers match {
    case Nil => Nil
    case 0::_ => Nil
    case x::xs if xs.size < x => (offset :: acc).reverse
    case x::xs =>
      val jump = findMaxIndexPlusValue(xs take x)
      findHops(xs drop jump, offset :: acc, offset + jump + 1)
  }

  private def findMaxIndexPlusValue(numbers: List[Int]): Int =
    numbers.zipWithIndex.maxBy { case (v, i) => v + i }._2

  private def readNumbers(): List[Either[NumberFormatException, Int]] =
    Iterator.continually(StdIn.readLine()).takeWhile(_ != null).map(toNonNegativeInt).toList

  private def toNonNegativeInt(number: String): Either[NumberFormatException, Int] =
    try {
      val int = number.toInt
      if (int < 0) Left(new NumberFormatException("negative number")) else Right(int)
    } catch {
      case nfe: NumberFormatException => Left(nfe)
    }

  private def prepareOutput(numbers: List[Int]): String = numbers match {
    case Nil => "failure"
    case xs => xs.mkString("", ", ", ", out")
  }

}
