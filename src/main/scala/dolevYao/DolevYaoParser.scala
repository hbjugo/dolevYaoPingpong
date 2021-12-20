package dolevYao

import java.io.{BufferedReader, InputStreamReader}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.*

object DolevYaoParser extends StandardTokenParsers {

  lexical.reserved ++= List("Er", "Es", "Ez", "Dr", "Ds", "Dz", "dr", "ds", "dz", "ir", "is", "iz", "d")

  def operation: Parser[Operation] =
    rep(uniqueOperation) ^^ (_.foldRight(Identity.asInstanceOf[Operation])((o1, o2) =>
      // FIXME
      (o1, o2) match
        case (Encrypt(p, Identity), _) => Encrypt(p, o2)
        case (Decrypt(p, Identity), _) => Decrypt(p, o2)
        case (Desindex(p, Identity), _) => Desindex(p, o2)
        case (Index(p, Identity), _) => Index(p, o2)
        case (Identity, _) => o2
        case (Delete(Identity), _) => Delete(o2)
        case _ => throw UnknownOperation(o1)
    ))

  def uniqueOperation: Parser[Operation] =
    "Er" ^^^ Encrypt(R, Identity) |
    "Es" ^^^ Encrypt(S, Identity) |
    "Ez" ^^^ Encrypt(Z, Identity) |
    "Dr" ^^^ Decrypt(R, Identity) |
    "Ds" ^^^ Decrypt(S, Identity) |
    "Dz" ^^^ Decrypt(Z, Identity) |
    "dr" ^^^ Desindex(R, Identity) |
    "ds" ^^^ Desindex(S, Identity) |
    "dz" ^^^ Desindex(Z, Identity) |
    "ir" ^^^ Index(R, Identity) |
    "is" ^^^ Index(S, Identity) |
    "iz" ^^^ Index(Z, Identity) |
    "d"  ^^^ Delete(Identity) |
    "λ"  ^^^ Identity

  case class UnknownOperation(operation: Operation) extends Exception(operation.toString)

  case class NoRuleApplies(operation: Operation) extends Exception(operation.toString)

  def reduce(operation: Operation): Operation =
    operation match
      case Encrypt(participant1, Decrypt(participant2, operation)) if (participant1 == participant2) => operation
      case Decrypt(participant1, Encrypt(participant2, operation)) if (participant1 == participant2) => operation
      case Desindex(participant1, Index(participant2, operation)) if (participant1 == participant2) => operation
      case Delete(Index(_, operation)) => operation
      case Encrypt(participant, operation) => Encrypt(participant, reduce(operation))
      case Decrypt(participant, operation) => Decrypt(participant, reduce(operation))
      case Desindex(participant, operation) => Desindex(participant, reduce(operation))
      case Index(participant, operation) => Index(participant, reduce(operation))
      case Delete(operation) => Delete(reduce(operation))
      case Identity => throw NoRuleApplies(operation)


  def path(o: Operation, reduce: Operation => Operation): LazyList[Operation] =
    try
      var t1 = reduce(o)
      LazyList.cons(o, path(t1, reduce))
    catch
      case NoRuleApplies(_) =>
        LazyList.cons(o, LazyList.empty)

  def parseString(s: String): Option[Operation] = {
    val tokens = new lexical.Scanner(s)
    phrase(operation)(tokens) match
      case Success(trees, _) =>
        try {
          // Extract the reduction path.
          val reductionPath = path(trees, reduce)
          // Return the most reduced.
          return Some(reductionPath.last)
        }
        catch
          case tpError: Exception => println(tpError.toString)

      case e => println(e)
      return None
  }

  def main(args: Array[String]): Unit =
    val stdin = new BufferedReader(new InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(operation)(tokens) match
      case Success(trees, _) =>
        try
          for (o <- path(trees, reduce))
            println(o)
        catch
          case tpError: Exception => println(tpError.toString)

      case e => println(e)

}
