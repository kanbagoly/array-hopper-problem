package com.kanbagoly.arrayhopper

object ArrayHopper {

  def main(args: Array[String]): Unit = {

  }

  /*
    private static List<Integer> findHops(List<Integer> numbers) {
        if (numbers.isEmpty())
            return Collections.emptyList();
        List<Integer> hops = new ArrayList<>(Collections.singletonList(0));
        int from = 0;
        while (from + numbers.get(from) < numbers.size() && numbers.get(from) != 0) {
            List<Integer> nextPlaces = numbers.subList(from + 1, Math.min(from + numbers.get(from) + 1, numbers.size()));
            from += findNextHop(nextPlaces) + 1;
            hops.add(from);
        }
        return from + numbers.get(from) >= numbers.size() ? hops : Collections.emptyList();
    }

      private static Integer findNextHop(List<Integer> numbers) {
        int max = 0;
        for (int i = 0; i < numbers.size(); i++) {
            if (max + numbers.get(max) < i + numbers.get(i)) {
                max = i;
            }
        }
        return max;
    }*/

  def findHops(numbers: List[Int], base: Int = 0): List[Int] = numbers match {
    case Nil => Nil
    case 0::_ => Nil
    case x::xs => base :: findHops(xs, base + x)
  }

  private def findHops2(numbers: List[Int], base: Int, hops: List[Int]): List[Int] = numbers match {
    case Nil => hops
    case x::xs =>
      val hop = findMaxIndexPlusValue(xs.slice(0, (base + x).max(xs.size)))
      findHops2(xs, base + hop, base :: hops.drop(hop))
  }

  private def findMaxIndexPlusValue(numbers: List[Int]): Int =
    numbers.zipWithIndex.maxBy { case (v, i) => v + i }._1

}
