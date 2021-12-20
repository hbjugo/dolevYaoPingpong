package dolevYao

import scala.annotation.tailrec

// Checks whether the given operation can be performed by the given sender.
@tailrec
def isValidOperation(sender: Participant, operation: Operation): Boolean = operation match {
  // Encryption keys are public.
  case Encrypt(_, innerOperation) => isValidOperation(sender, innerOperation)
  // Insertion & deletion for all participants are allowed.
  case Desindex(_, innerOperation) => isValidOperation(sender, innerOperation)
  case Index(_, innerOperation) => isValidOperation(sender, innerOperation)
  // Decryption keys are only known by the corresponding sender.
  case Decrypt(p, innerOperation) =>
    (p == sender) && isValidOperation(sender, innerOperation)
  case Delete(innerOperation) => isValidOperation(sender, innerOperation)
  // Base case.
  case Identity => true
}

// For a given operation, the given sender is replaced by X and the other party is replaced by Y.
def replayedOperation(operation: Operation, oldSender: Participant, newSender: Participant, newReceiver: Participant): Operation = {
  def replaced(p: Participant): Participant = if p == oldSender then newSender else newReceiver
  operation match {
    case Encrypt(p, innerOperation) => Encrypt(replaced(p), replayedOperation(innerOperation, oldSender, newSender, newReceiver))
    case Decrypt(p, innerOperation) => Decrypt(replaced(p), replayedOperation(innerOperation, oldSender, newSender, newReceiver))
    case Desindex(p, innerOperation) => Desindex(replaced(p), replayedOperation(innerOperation, oldSender, newSender, newReceiver))
    case Index(p, innerOperation) => Index(replaced(p), replayedOperation(innerOperation, oldSender, newSender, newReceiver))
    case Delete(innerOperation) => Delete(replayedOperation(innerOperation, oldSender, newSender, newReceiver))
    // Base case.
    case Identity => Identity
  }
}

class ProtocolStep(val sender: Participant, val operation: Operation) {
  // Returns whether the step is valid, i.e., the operations can be performed by the participant.
  def isValidStep: Boolean = isValidOperation(sender, operation)
  // Returns a new protocol step where the senders and receivers are replaced according to the given parameters.
  def replayedStep(newSender: Participant, newReceiver: Participant): ProtocolStep = {
    val newOperation = replayedOperation(operation, sender, newSender, newReceiver)
    ProtocolStep(newSender, newOperation)
  }
  override def toString: String = sender.toString + ": " + operation
}

class Protocol(val steps: List[ProtocolStep]) {
  // Returns whether all the steps in the protocol are valid and there is at least one step.
  def isValidProtocol: Boolean = steps.nonEmpty && steps.map(_.isValidStep).reduce((a, b) => a && b)
  override def toString: String = "Protocol =>\n" + steps.map(s => "\t" + s).mkString("\n")
}
