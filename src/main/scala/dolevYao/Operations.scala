package dolevYao

import scala.util.parsing.input.Positional

sealed abstract class Operation extends Positional {
  def getOperator(): Operator
}

case class Encrypt(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "E" + participant + " " + operation
  override def getOperator() = EncryptOperator(participant)
}

case class Decrypt(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "D" + participant + " " + operation
  override def getOperator() = DecryptOperator(participant)
}

case class Index(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "i" + participant + " " + operation
  override def getOperator() = IndexOperator(participant)
}

case class Desindex(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "d" + participant + " " + operation
  override def getOperator() = DesindexOperator(participant)
}

case class Delete(operation: Operation) extends Operation {
  override def toString = "d " + operation
  override def getOperator() = DeleteOperator()
}

object Identity extends Operation {
  override def toString: String = "λ " 
  override def getOperator() = IdentityOperator()
}
