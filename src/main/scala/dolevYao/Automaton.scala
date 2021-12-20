package dolevYao

class Automaton {

  var edges: Set[(Int, Int, Operator)] = Set()
  var lastIntermediateState: Int = 0

  // Returns a new intermediate state.
  private def newIntermediateState: Int = {
    lastIntermediateState += 1
    lastIntermediateState - 1
  }

  // Converts an operation to a label. Since the operations are defined as a linked list, we only consider the head.
  private def operationEdgeRepresentation(op: Operation) = op.getOperator()


  // (startState = a, finalState = b, op) => (a, 1, op[0]), (1, 2, op[1]), ... , (n-1, n, op[n-2]), (n, b, op[n-1])
  private def buildPath(startState: Int, finalState: Int, operation: Operation): List[(Int, Int, Operator)] = {
    val edgeLabel = operationEdgeRepresentation(operation)
    val lastOperation = operation match {
      case Encrypt(_, innerOperation) => innerOperation == Identity
      case Decrypt(_, innerOperation) => innerOperation == Identity
      case Index(_, innerOperation) => innerOperation == Identity
      case Desindex(_, innerOperation) => innerOperation == Identity
      case Delete(innerOperation) => innerOperation == Identity
      // Cannot build a path from an identity operation.
      case Identity => return List.empty
    }
    if lastOperation then (startState, finalState, edgeLabel) :: Nil else {
      val nextState = newIntermediateState
      operation match {
        case Encrypt(participant, innerOperation) =>
          (startState, nextState, edgeLabel) :: buildPath(nextState, finalState, innerOperation)
        case Decrypt(participant, innerOperation) =>
          (startState, nextState, edgeLabel) :: buildPath(nextState, finalState, innerOperation)
        case Index(participant, innerOperation) =>
          (startState, nextState, edgeLabel) :: buildPath(nextState, finalState, innerOperation)
        case Desindex(participant, innerOperation) =>
          (startState, nextState, edgeLabel) :: buildPath(nextState, finalState, innerOperation)
        case Delete(innerOperation) =>
          (startState, nextState, edgeLabel) :: buildPath(nextState, finalState, innerOperation)
      }
    }
  }

  // Adds a new path to the automaton from the fromState to the toState with the intermediate states generated
  // according to the given operation.
  def addNewPath(fromState: Int, toState: Int, operation: Operation): List[(Int, Int, Operator)] = {
    // Make sure that the newly generated intermediate states will be at least one larger than toState.
    if lastIntermediateState <= toState then lastIntermediateState = toState + 1
    // Generate the path.
    val generatedPath = buildPath(fromState, toState, operation)
    edges = edges ++ generatedPath
    generatedPath
  }

  // Returns whether a direct edge from fromState to toState exists with the given label.
  def edgeExists(fromState: Int, toState: Int, label: Operator): Boolean = edges.contains((fromState, toState, label))

  // Get the label for an edge
  def edgeLabel(fromState: Int, toState: Int): List[Operator] = 
    edges
      .filter { case (from, to, label) => from == fromState && to == toState }
      .map { case (from, to, label) => label }.toList
  // Returns the number of states.
  // Assuming that states are successive, starting from 0.
  def numStates: Int = edges.flatMap(triplet => List(triplet._1, triplet._2)).max + 1

  // Returns an empty matrix representing SxS where S is the set of states.
  // Assuming that states are successive, starting from 0.
  def extractMatrix: Array[Array[Boolean]] = Array.ofDim(numStates + 1, numStates + 1)

  // For a given i, finds an edge (k, i, label) for some k, label.
  def findWithNextState(nextState: Int): Option[(Int, Int, Operator)] = edges.find(e => e._2 == nextState)

  // For a given j, finds an edge (j, l, label) for some l, label.
  def findWithPrevState(prevState: Int): Option[(Int, Int, Operator)] = edges.find(e => e._1 == prevState)

  override def toString: String = edges.map(p => p.toString()).mkString("\n")
}

object AutomatonTest {
  def main(args: Array[String]): Unit = {
    val a = Automaton()
    println(a.addNewPath(0, 1, Encrypt(R, Decrypt(R, Desindex(S, Identity)))))
  }
}
