package dolevYao

class Automaton {

  var paths: Set[(Int, Int, String)] = Set()
  var lastIntermediateState: Int = 0

  private def newIntermediateState: Int = {
    lastIntermediateState += 1
    lastIntermediateState - 1
  }

  private def operationEdgeRepresentation(op: Operation): String = {
    op match {
      case Encrypt(participant, _) => "E" + participant
      case Decrypt(participant, _) => "D" + participant
      case Index(participant, _) => "i" + participant
      case Desindex(participant, _) => "d" + participant
      case Identity => Identity.toString
    }
  }

  // (startState = a, finalState = b, op) => (a, 1, op[0]), (1, 2, op[1]), ... , (n-1, n, op[n-2]), (n, b, op[n-1])
  private def buildPath(startState: Int, finalState: Int, operation: Operation): List[(Int, Int, String)] = {
    val edgeLabel = operationEdgeRepresentation(operation)
    val lastOperation = operation match {
      case Encrypt(_, innerOperation) => innerOperation == Identity
      case Decrypt(_, innerOperation) => innerOperation == Identity
      case Index(_, innerOperation) => innerOperation == Identity
      case Desindex(_, innerOperation) => innerOperation == Identity
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
      }
    }
  }

  // Adds a new path to the automaton from the fromState to the toState with the intermediate states generated
  // according to the given operation.
  def addNewPath(fromState: Int, toState: Int, operation: Operation): List[(Int, Int, String)] = {
    // Make sure that the newly generated intermediate states will be at least one larger than toState.
    if lastIntermediateState <= toState then lastIntermediateState = toState + 1
    // Generate the path.
    val generatedPath = buildPath(fromState, toState, operation)
    // If the operation is a valid operation for Z, then it ca
    paths = paths ++ generatedPath
    generatedPath
  }
  // Returns whether a direct path from fromState to toState exists with the given label.
  def pathExists(fromState: Int, toState: Int, label: String): Boolean = paths.contains((fromState, toState, label))
  override def toString: String = paths.map(p => p.toString()).mkString("\n")
}

object AutomatonTest {
  def main(args: Array[String]): Unit = {
    val a = Automaton()
    println(a.addNewPath(0, 1, Encrypt(R, Decrypt(R, Desindex(S, Identity)))))
  }
}
