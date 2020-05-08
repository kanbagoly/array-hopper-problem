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

  // TODO: Is it possible to remove base? (and use acc.head)
  // TODO: case Nil => Nil can be extracted?
  def findHops(numbers: List[Int], acc: List[Int] = Nil, base: Int = 0): List[Int] = numbers match {
    case Nil => Nil
    case 0::_ => Nil
    case x::Nil => (base :: acc).reverse
    case x::xs =>
      val hop = findMaxIndexPlusValue(xs.slice(0, (base + x).min(xs.size)))
      findHops(xs.drop(hop), base :: acc, base + hop + 1)
  }

  private def findMaxIndexPlusValue(numbers: List[Int]): Int =
    numbers.zipWithIndex.maxBy { case (v, i) => v + i }._2

}
