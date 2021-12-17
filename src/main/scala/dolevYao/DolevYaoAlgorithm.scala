package dolevYao

object dolevYaoAlgorithm {

  type MatrixVertices = Set[Int]
  type MatrixEdges = Int
  type Matrix = (MatrixVertices, MatrixEdges)

  def constructMatrix(operation: Operation): Matrix =
    val vertices = constructVertices(operation)
    (vertices, constructEdges(operation, vertices))

  def constructVertices(operation: Operation): MatrixVertices = ???
  def constructEdges(operation: Operation, matrixVertices: MatrixVertices): MatrixEdges = ???

  def isCorrect(matrix: Matrix): Boolean = ???
  def constructCollapsingRelation(matrix: Matrix): Matrix = ???
}
