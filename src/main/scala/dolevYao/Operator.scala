package dolevYao

sealed abstract class Operator {
  def apply(next: Operation) = operation(next)
  def operation(next: Operation): Operation 
}

case class EncryptOperator(participant: Participant) extends Operator {
  override def toString = "E" + participant
  override def operation(next: Operation) = Encrypt(participant, next)
}

case class DecryptOperator(participant: Participant) extends Operator {
  override def toString = "D" + participant
  override def operation(next: Operation) = Decrypt(participant, next)
}
case class IndexOperator(participant: Participant) extends Operator {
  override def toString = "i" + participant
  override def operation(next: Operation) = Index(participant, next)
}
case class DesindexOperator(participant: Participant) extends Operator {
  override def toString = "d" + participant
  override def operation(next: Operation) = Desindex(participant, next)
}
case class DeleteOperator() extends Operator {
  override def toString = "d"
  override def operation(next: Operation) = Delete(next)
}
case class IdentityOperator() extends Operator {
  override def toString: String = Identity.toString
  override def operation(next: Operation) = Identity
}
