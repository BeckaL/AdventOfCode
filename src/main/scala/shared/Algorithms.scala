package shared

import scala.annotation.tailrec

//TODO: make this faster => using min by to determine next isn't the quickest
//Written for day 17 2023, may need some more tweaking to make generic
def dijkstra[A](getWeight: (A, Int) => Int, next: (A, Set[A]) => List[A], isTarget: A => Boolean, starts: Set[A]): Int =
  @tailrec
  def go(visited: Set[A], toVisit: Map[A, Int]): Int =
    val (nextNode, weight) = toVisit.minBy(_._2)
    if (isTarget(nextNode))
      weight
    else
      val followingNodes = next(nextNode, visited)
      val updatedToVisit = followingNodes.map(n => n -> getWeight(n, weight)).foldLeft(toVisit.removed(nextNode)) { case (newToVisit, (node, newWeight)) =>
        if (newWeight < toVisit.getOrElse(node, Int.MaxValue))
          newToVisit.updated(node, newWeight)
        else
          newToVisit
      }
      go(visited ++ Set(nextNode), updatedToVisit)

  go(Set(), starts.map(node => node -> 0).toMap)