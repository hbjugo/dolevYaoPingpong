package dolevYao

import scala.util.parsing.input.Positional

sealed abstract class Operation extends Positional

case class Encrypt(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "E" + participant + " " + operation
}

case class Decrypt(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "D" + participant + " " + operation
}

case class Index(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "i" + participant + " " + operation
}

case class Desindex(participant: Participant, operation: Operation) extends Operation {
  override def toString: String = "d" + participant + " " + operation
}

object Identity extends Operation {
  override def toString: String = "λ " 
}
