package qurator

import cats.effect.IO
import qurator.domain.device.Device
import qurator.testbed.SchedulerBenchmarkRunner
import qurator.testbed.SchedulerBenchmarkRunner.{
  BaselineCandidate,
  BaselinePlanningState,
  BaselinePolicy
}
import weaver.SimpleIOSuite

object SchedulerBenchmarkBaselineSuite extends SimpleIOSuite {

  private def device(id: String): Device =
    Device(
      platform = "test",
      platformId = id,
      qubits = 8,
      t1 = 0.0f,
      t2 = 0.0f,
      gateSet = Nil
    )

  private def candidate(
      id: String,
      queueWaitMillis: Long,
      runMillis: Long,
      fidelity: Double
  ): BaselineCandidate =
    BaselineCandidate(
      device = device(id),
      queueWaitMillis = queueWaitMillis,
      runMillis = runMillis,
      predictedLogFidelity = math.log(fidelity),
      predictedSuccessProbability = fidelity
    )

  test("shortest queue target-fidelity rejects a shorter low-fidelity device") {
    val lowFidelity = candidate("short-low-fidelity", 10L, 100L, 0.89)
    val targetDevice = candidate("longer-target-device", 50L, 100L, 0.91)

    val selected =
      SchedulerBenchmarkRunner.selectBaselineCandidate(
        BaselinePolicy.ShortestQueueTargetFidelity(0.9),
        List(lowFidelity, targetDevice),
        BaselinePlanningState()
      )

    IO.pure(expect(selected.exists(_.device.platformId == "longer-target-device")))
  }

  test("shortest queue target-fidelity fails when no device meets the target") {
    val selected =
      SchedulerBenchmarkRunner.selectBaselineCandidate(
        BaselinePolicy.ShortestQueueTargetFidelity(0.95),
        List(candidate("device-a", 10L, 100L, 0.90)),
        BaselinePlanningState()
      )

    IO.pure(expect(selected.isLeft))
  }

  test("shortest queue target-fidelity includes earlier assignments in projected queue length") {
    val state =
      BaselinePlanningState(
        projectedRuntimeMillisByDevice = Map("device-a" -> 500L)
      )

    val selected =
      SchedulerBenchmarkRunner.selectBaselineCandidate(
        BaselinePolicy.ShortestQueueTargetFidelity(0.9),
        List(
          candidate("device-a", 0L, 100L, 0.99),
          candidate("device-b", 100L, 100L, 0.90)
        ),
        state
      )

    IO.pure(expect(selected.exists(_.device.platformId == "device-b")))
  }

  test("quantum list scheduling minimizes projected completion time") {
    val state =
      BaselinePlanningState(
        projectedRuntimeMillisByDevice = Map("device-a" -> 500L)
      )

    val selected =
      SchedulerBenchmarkRunner.selectBaselineCandidate(
        BaselinePolicy.QuantumListScheduling,
        List(
          candidate("device-a", 0L, 100L, 0.99),
          candidate("device-b", 100L, 100L, 0.90)
        ),
        state
      )

    IO.pure(expect(selected.exists(_.device.platformId == "device-b")))
  }

  test("fair share selects the eligible device with the least predicted QPU usage") {
    val state =
      BaselinePlanningState(
        fairShareUsageMillisByDevice = Map(
          "device-a" -> 500L,
          "device-b" -> 100L
        )
      )

    val selected =
      SchedulerBenchmarkRunner.selectBaselineCandidate(
        BaselinePolicy.FairShare,
        List(
          candidate("device-a", 0L, 100L, 0.99),
          candidate("device-b", 200L, 100L, 0.90)
        ),
        state
      )

    IO.pure(expect(selected.exists(_.device.platformId == "device-b")))
  }

  test("planning state accounts for assigned QPU time") {
    val selected = candidate("device-a", 0L, 250L, 0.9)
    val updated =
      SchedulerBenchmarkRunner.advanceBaselinePlanningState(
        BaselinePlanningState(),
        selected
      )

    IO.pure(
      expect(updated.projectedRuntimeMillisByDevice.get("device-a").contains(250L)) and
        expect(updated.fairShareUsageMillisByDevice.get("device-a").contains(250L))
    )
  }
}
