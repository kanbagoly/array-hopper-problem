package com.kanbagoly.arrayhopper

import scala.annotation.tailrec

object ArrayHopper {

  def main(args: Array[String]): Unit = {

  }

  @tailrec
  def findHops(numbers: List[Int], acc: List[Int] = Nil, offset: Int = 0): List[Int] = numbers match {
    case Nil => Nil
    case 0::_ => Nil
    case x::xs if xs.size < x => (offset :: acc).reverse
    case x::xs =>
      val hop = findMaxIndexPlusValue(xs take x)
      findHops(xs drop hop, offset :: acc, offset + hop + 1)
  }

  private def findMaxIndexPlusValue(numbers: List[Int]): Int =
    numbers.zipWithIndex.maxBy { case (v, i) => v + i }._2

}
