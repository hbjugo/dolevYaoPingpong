package dolevYao

import scala.annotation.tailrec

// Checks whether the given operation can be performed by the given participant.
@tailrec
def isValidOperation(operation: Operation, participant: Participant): Boolean = operation match {
  // Encryption keys are public.
  case Encrypt(_, innerOperation) => isValidOperation(innerOperation, participant)
  // Insertion & deletion for all participants are allowed.
  case Desindex(_, innerOperation) => isValidOperation(innerOperation, participant)
  case Index(_, innerOperation) => isValidOperation(innerOperation, participant)
  // Decryption keys are only known by the corresponding participant.
  case Decrypt(operationParticipant, innerOperation) =>
    (operationParticipant == participant) && isValidOperation(innerOperation, participant)
  // Base case.
  case Identity => true
}

class ProtocolStep(val participant: Participant, val operation: Operation) {
  // Returns whether the step is valid, i.e., the operations can be performed by the participant.
  def isValidStep: Boolean = isValidOperation(operation, participant)
  override def toString: String = "Step => " + participant + ": " + operation
}

class Protocol(val steps: List[ProtocolStep]) {
  // Returns whether all the steps in the protocol are valid.
  def isValidProtocol: Boolean = steps.map(_.isValidStep).reduce((a, b) => a && b)
  override def toString: String = "Protocol =>\n" + steps.map(s => "\t" + s).mkString("\n")
}
