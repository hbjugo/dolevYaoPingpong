package dolevYao

import java.io.{BufferedReader, InputStreamReader}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.*
import dolevYao.ProtocolStep

object Main {
  def main(args: Array[String]): Unit = {
    // Read the protocol from the standard input.
    val stdin = new BufferedReader(new InputStreamReader(System.in))
    val protocolInput = LazyList.from(0)
      // Alternate between S and R for each step.
      .map(step => if step % 2 == 0 then S else R)
      // Read input for each step.
      .map(participant => {
        print("Enter step for " + participant + " (newline to terminate): ")
        (participant, stdin.readLine())
      })
      // Until a newline is presented.
      .takeWhile((_, input) => input.nonEmpty)
      .toList
    println("Read protocol: " + protocolInput)
    print("Parsing...")
    // Parse the operations and convert into a protocol step.
    val protocolSteps: List[ProtocolStep] = protocolInput.map((participant, input) =>
      ProtocolStep(participant, DolevYaoParser.parseString(input).getOrElse(Identity)))
    println("OK!")
    println("Validating...")
    // Check whether all the steps are valid.
    for(step <- protocolSteps) {
      println(step.toString + "\n\tVALID: " + step.isValidStep)
    }
    // Construct the protocol.
    val protocol = Protocol(protocolSteps)
    if(!protocol.isValidProtocol) {
      println("Invalid protocol. Aborting.")
      return
    }
    println("OK!")
  }
}
