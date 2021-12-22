package dolevYao

import scala.collection.mutable

object DolevYaoAlgorithm {

  def reduceAll(operation: Operation): Operation =
    val reductionPath = DolevYaoParser.path(operation, DolevYaoParser.reduce)
    return reductionPath.last

  // Returns whether the protocol is secure or not and the collapsing path if it is not secure.
  def isSecure(automaton: Automaton): (Boolean, Option[List[Operator]]) =
    val n = automaton.numStates
    println(f"n = ${n}")
    val collapsingRelation = CollapsingRelation(n)

    val queue = mutable.Queue[(Int, Int)]()
    queue.addAll(Range(0, n).map(i => (i,i)))

    while (queue.nonEmpty) {
      // Delete the first pair, (i, j), from Q .
      val (i,j) = queue.dequeue

      // 2) if (j, k) in C and (i, k) not in C then put (i, k) in C and in Q.
      Range(0, n)
        .filter(k => collapsingRelation.in(j, k) && !collapsingRelation.in(i, k))
        .map(k => {
          collapsingRelation.extendRight(i, j, k)
          queue.enqueue((i,k))
        })

      // 3) If (k, i) in C and (k, j) not in C then put (k, j) in C and in Q.
      Range(0, n)
        .filter(k => collapsingRelation.in(k, i) && !collapsingRelation.in(k, j))
        .map(k => {
          collapsingRelation.extendLeft(i, j, k)
          queue.enqueue((k,j))
        })

      // 4) If
      //    (k, l) not in C and
      //    k ->^(sigma) i and
      //    j ->^(theta) l and
      //    sigma theta = lambda [is one of the cancellation rules]
      //    then put (k, l) in C and in Q.
      (for k <- Range(0, n);
          l <- Range(0, n) if !collapsingRelation.in(k, l)
          yield (k,l)
      ).foreach{ case (k,l) =>
        val sigmas: List[Operator] = automaton.edgeLabel(k, i)
        val thetas: List[Operator] = automaton.edgeLabel(j, l)
        val zipped: List[(Operator, Operator)] = (for sigma <- sigmas;
          theta <- thetas
          yield (sigma,theta)
        )
        val found: Option[(Operator, Operator)] = zipped.find {
          case (sigma, theta) =>
            val op = sigma(theta(Identity))
            //println(op)
            val res: Boolean = reduceAll(op) == Identity
            res
        }
        found.map { 
          case (sigma, theta) =>
            //println(f"${i}, ${j}, ${k}, ${l}")
            collapsingRelation.extendBoth(i, j, k, l, sigma, theta)
            queue.enqueue((k,l))
        }
      }
    }
    collapsingRelation.membership.foreach(a =>
      a.foreach(b => print(if b then "1 " else "  "))
      println()
    )
    // Return whether the protocol is secure (i.e., there is not a collapsing path from 0 -> 1) and the
    // path from 0 -> 1 if the protocol is insecure.
    (!collapsingRelation.in(0, 1), collapsingRelation.getPath(0, 1))
}
