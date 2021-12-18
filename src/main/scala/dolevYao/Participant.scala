package dolevYao

sealed abstract class Participant

object R extends Participant {
  override def toString: String = "R"
}
object S extends Participant {
  override def toString: String = "S"
}
object Z extends Participant {
  override def toString: String = "Z"
}