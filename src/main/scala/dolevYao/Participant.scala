package dolevYao

sealed abstract class Participant

object R extends Participant {
  override def toString: String = "r"
}
object S extends Participant {
  override def toString: String = "s"
}
object Z extends Participant {
  override def toString: String = "z"
}