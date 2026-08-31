package qurator

import cats.effect.IO
import qurator.domain.circuit._
import qurator.programs.Scheduler
import weaver.SimpleIOSuite

object SchedulerMergeCircuitsSuite extends SimpleIOSuite {

  test("mergeCircuits shifts named-gate qubits") {
    val first = Circuit(List(H(0)), qubits = 2)
    val second = Circuit(
      List(NamedGate("rzz", Vector("-0.9272952180016123"), Vector(0, 3))),
      qubits = 4
    )

    val merged = Scheduler.mergeCircuits(List(first, second))

    IO.pure(expect.all(
      merged.qubits == 6,
      merged.remainingGates == List(
        H(0),
        NamedGate("rzz", Vector("-0.9272952180016123"), Vector(2, 5))
      )
    ))
  }
}
