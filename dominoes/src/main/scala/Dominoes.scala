import scala.annotation.tailrec

object Dominoes {
  private type Domino = (Int, Int)

  private def normalize(domino: Domino): Domino = if (domino._1 > domino._2) domino.swap else domino

  def chain(dominoes: List[Domino]): Option[List[Domino]] = {
    if (dominoes.isEmpty) return Some(List())

    // count of normalized dominoes.
    val dominoCount: Map[Domino, Int] =
      dominoes.groupBy(normalize).view.mapValues(_.size).toMap

    // possible candidates given one side of a domino.
    val connections: Map[Int, List[Int]] =
      (dominoes ++ dominoes.map(_.swap)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

    // backtrack is deep first search.
    @tailrec
    def backtrack(current: Domino, count: Map[Domino, Int], acc: List[Domino]): Option[List[Domino]] = {
      val newAcc: List[Domino] = current :: acc

      if (count.values.sum == 0) {
        val (head, last) = (newAcc.head, newAcc.last)
        val result: Option[List[Domino]] = if (head._1 == last._2) Some(newAcc) else None
        return result
      }

      // always add to head, thus _1.
      val a: Int = current._1
      val normalizedNext: Option[Domino] = connections(a).map(b => normalize((a, b))).find(count(_) > 0)

      if (normalizedNext.isEmpty) None
      else {
        val next: Domino = normalizedNext.get
        val newCount: Map[Domino, Int] = count.updated(next, count(next) - 1)
        backtrack(if (next._2 == a) next else next.swap, newCount, newAcc)
      }
    }

    val current: Domino = normalize(dominoes.head)
    backtrack(current, dominoCount.updated(current, dominoCount(current) - 1), List())
  }

}
