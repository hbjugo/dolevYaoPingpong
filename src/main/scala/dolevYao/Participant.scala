package dolevYao

sealed abstract class Participant

object R extends Participant {
  override def toString: String = "dolevYao.R"
}
object S extends Participant {
  override def toString: String = "dolevYao.S"
}
object Z extends Participant {
  override def toString: String = "dolevYao.Z"
}