import scala.annotation.tailrec

object Dominoes {
  private type Domino = (Int, Int)

  private def normalize(domino: Domino): Domino = if (domino._1 > domino._2) domino.swap else domino

  def chain(dominoes: List[Domino]): Option[List[Domino]] = {
    if (dominoes.isEmpty) return Some(List())

    val dominoCount: Map[Domino, Int] =
      dominoes.groupBy(normalize).view.mapValues(_.size).toMap

    val dominoesByOne: Map[Int, List[Domino]] =
      (dominoes ++ dominoes.map(_.swap)).groupBy(_._1).view.mapValues(_.map(normalize)).toMap

    @tailrec
    def backtrack(current: Domino, count: Map[Domino, Int], acc: List[Domino]): Option[List[Domino]] = {
      val normalizedCurrent = normalize(current)
      val newCount = count.updated(normalizedCurrent, count(normalizedCurrent) - 1)
      val newAcc = current :: acc

      if (newCount.values.sum == 0) {
        val (head, last) = (newAcc.head, newAcc.last)
        val result = if (head._1 == last._2) Some(newAcc) else None
        return result
      }

      // always add to head, thus _1.
      val normalizedNext: Option[Domino] = dominoesByOne(current._1).find(newCount(_) > 0)

      if (normalizedNext.isEmpty) None else {
        val next = normalizedNext.get
        backtrack(if (current._1 == next._2) next else next.swap, newCount, newAcc)
      }
    }

    backtrack(dominoes.head, dominoCount, List())
  }
}