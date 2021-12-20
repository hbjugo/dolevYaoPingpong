package dolevYao

import scala.collection.mutable

object dolevYaoAlgorithm {
  def reduceAll(operation: Operation): Operation =
    val reductionPath = DolevYaoParser.path(operation, DolevYaoParser.reduce)
    return reductionPath.last


  def isSecure(automaton: Automaton): Boolean =
    val n = automaton.numStates
    println(f"n = ${n}")
    val collapsingRelation = Range(0, n).map(i =>
        Range(0, n).map(j => j == i).toArray
      ).toArray

    val queue = mutable.Queue[(Int, Int)]()
    queue.addAll(Range(0, n).map(i => (i,i)))

    while (!queue.isEmpty) {
      // Delete the first pair, (i, j), from Q .
      val (i,j) = queue.dequeue

      // 2) if (j, k) in C and (i, k) not in C then put (i, k) in C and in Q.
      Range(0, n)
        .filter(k => collapsingRelation(j)(k) && !collapsingRelation(i)(k))
        .map(k => {
          collapsingRelation(i)(k) = true
          queue.enqueue((i,k))
        })

      // 3) If (k, i) in C and (k, j) not in C then put (k, j) in C and in Q.
      Range(0, n)
        .filter(k => collapsingRelation(k)(i) && !collapsingRelation(k)(j))
        .map(k => {
          collapsingRelation(k)(j) = true
          queue.enqueue((k,j))
        })

      // 4) If
      //    (k, l) not in C and
      //    k ->^(sigma) i and
      //    j ->^(theta) l and
      //    sigma theta = lambda [is one of the cancellation rules]
      //    then put (k, l) in C and in Q.
      (for k <- Range(0, n);
          l <- Range(0, n) if !collapsingRelation(k)(l)
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
            collapsingRelation(k)(l) = true
            queue.enqueue((k,l))
        }
      }
    }
    collapsingRelation.foreach(a =>
      a.foreach(b => print(if b then "1 " else "  "))
      println()
    )

    ! collapsingRelation(0)(1)

  //def constructCollapsingRelation(matrix: Matrix): Matrix = ???
}
