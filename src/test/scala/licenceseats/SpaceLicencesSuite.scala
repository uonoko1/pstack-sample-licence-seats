package licenceseats

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

class SpaceLicencesSuite extends munit.FunSuite:

  private var requestCounter = 0
  private def freshId(): RequestId =
    requestCounter += 1
    RequestId(s"req-$requestCounter")

  private def user(n: Int): UserId = UserId(s"user-$n")

  private def fill(licences: SpaceLicences, count: Int): Unit =
    (1 to count).foreach(n => assert(licences.grant(freshId(), user(n)).isRight, s"grant $n"))

  /** Runs all tasks on their own threads, released at the same instant. */
  private def runTogether[A](tasks: Seq[() => A]): Seq[A] =
    val pool = Executors.newFixedThreadPool(tasks.size)
    given ExecutionContext = ExecutionContext.fromExecutor(pool)
    val ready = CountDownLatch(tasks.size)
    val start = CountDownLatch(1)
    try
      val futures = tasks.map { task =>
        Future {
          ready.countDown()
          start.await()
          task()
        }
      }
      assert(ready.await(10, TimeUnit.SECONDS), "workers did not start")
      start.countDown()
      Await.result(Future.sequence(futures), 30.seconds)
    finally pool.shutdownNow()

  test("plans have the specified seat limits") {
    assertEquals(Plan.Free.seatLimit, SeatLimit.Limited(10))
    assertEquals(Plan.Starter.seatLimit, SeatLimit.Limited(30))
    assertEquals(Plan.Standard.seatLimit, SeatLimit.Unlimited)
  }

  test("granting takes a seat and revoking frees it") {
    val licences = SpaceLicences(Plan.Free)
    assertEquals(licences.grant(freshId(), user(1)), Right(Applied(Plan.Free, 1)))
    assert(licences.isLicensed(user(1)))
    assertEquals(licences.revoke(freshId(), user(1)), Right(Applied(Plan.Free, 0)))
    assert(!licences.isLicensed(user(1)))
    assertEquals(licences.usedSeats, 0)
  }

  test("a grant beyond the seat limit is rejected") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    assertEquals(
      licences.grant(freshId(), user(11)),
      Left(Rejection.SeatLimitReached(Plan.Free, 10))
    )
    assertEquals(licences.usedSeats, 10)
    assert(!licences.isLicensed(user(11)))
  }

  test("a seat freed by a revoke can be granted again") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    assert(licences.revoke(freshId(), user(1)).isRight)
    assertEquals(licences.grant(freshId(), user(11)), Right(Applied(Plan.Free, 10)))
  }

  test("the Standard plan has no seat limit") {
    val licences = SpaceLicences(Plan.Standard)
    fill(licences, 500)
    assertEquals(licences.usedSeats, 500)
  }

  test("granting to a user who already holds a licence is rejected and takes no seat") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 1)
    assertEquals(licences.grant(freshId(), user(1)), Left(Rejection.AlreadyLicensed(user(1))))
    assertEquals(licences.usedSeats, 1)
  }

  test("revoking from a user without a licence is rejected") {
    val licences = SpaceLicences(Plan.Free)
    assertEquals(licences.revoke(freshId(), user(1)), Left(Rejection.NotLicensed(user(1))))
  }

  test("upgrading raises the seat limit") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    assertEquals(licences.changePlan(freshId(), Plan.Starter), Right(Applied(Plan.Starter, 10)))
    assertEquals(licences.grant(freshId(), user(11)), Right(Applied(Plan.Starter, 11)))
  }

  test("a downgrade is rejected when more seats are in use than the target plan offers") {
    val licences = SpaceLicences(Plan.Starter)
    fill(licences, 11)
    assertEquals(
      licences.changePlan(freshId(), Plan.Free),
      Left(Rejection.SeatsInUseExceedPlan(Plan.Free, 10, 11))
    )
    assertEquals(licences.plan, Plan.Starter)
  }

  test("a downgrade from the unlimited plan is checked against the target limit") {
    val licences = SpaceLicences(Plan.Standard)
    fill(licences, 31)
    assert(licences.changePlan(freshId(), Plan.Starter).isLeft)
    assertEquals(licences.plan, Plan.Standard)
  }

  test("a downgrade is accepted when the seats in use exactly fill the target plan") {
    val licences = SpaceLicences(Plan.Starter)
    fill(licences, 10)
    assertEquals(licences.changePlan(freshId(), Plan.Free), Right(Applied(Plan.Free, 10)))
    assertEquals(licences.grant(freshId(), user(11)), Left(Rejection.SeatLimitReached(Plan.Free, 10)))
  }

  test("resending a grant returns the same result and does not take a second seat") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    val first = licences.grant(id, user(1))
    assertEquals(first, Right(Applied(Plan.Free, 1)))
    // Without replay the resend would be rejected as AlreadyLicensed.
    assertEquals(licences.grant(id, user(1)), first)
    assertEquals(licences.usedSeats, 1)
  }

  test("a resend returns the original result even after the state has moved on") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    val first = licences.grant(id, user(99))
    assertEquals(first, Right(Applied(Plan.Free, 1)))
    fill(licences, 5)
    assertEquals(licences.grant(id, user(99)), first)
    assertEquals(licences.usedSeats, 6)
  }

  test("resending a revoke returns the original success and does not undo a later grant") {
    val licences = SpaceLicences(Plan.Free)
    assert(licences.grant(freshId(), user(1)).isRight)
    val id = freshId()
    val first = licences.revoke(id, user(1))
    assertEquals(first, Right(Applied(Plan.Free, 0)))
    assert(licences.grant(freshId(), user(1)).isRight)
    assertEquals(licences.revoke(id, user(1)), first)
    assert(licences.isLicensed(user(1)), "the resend must not revoke the newer licence")
  }

  test("resending a rejected grant returns the same rejection even after a seat is freed") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    val id = freshId()
    val first = licences.grant(id, user(11))
    assertEquals(first, Left(Rejection.SeatLimitReached(Plan.Free, 10)))
    assert(licences.revoke(freshId(), user(1)).isRight)
    assertEquals(licences.grant(id, user(11)), first)
    assertEquals(licences.usedSeats, 9)
  }

  test("resending a plan change returns the same result and does not reapply it") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    val first = licences.changePlan(id, Plan.Starter)
    assert(licences.changePlan(freshId(), Plan.Standard).isRight)
    assertEquals(licences.changePlan(id, Plan.Starter), first)
    assertEquals(licences.plan, Plan.Standard)
  }

  test("reusing a request ID for a different command is rejected and changes nothing") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    assert(licences.grant(id, user(1)).isRight)
    assertEquals(
      licences.grant(id, user(2)),
      Left(Rejection.RequestIdReused(id, Command.Grant(user(1))))
    )
    assertEquals(licences.holders, Set(user(1)))
    // The original request is still replayable afterwards.
    assertEquals(licences.grant(id, user(1)), Right(Applied(Plan.Free, 1)))
  }

  test("concurrent grants never exceed the seat limit") {
    (1 to 20).foreach { round =>
      val licences = SpaceLicences(Plan.Starter)
      val results = runTogether((1 to 100).map { n => () =>
        licences.grant(RequestId(s"round-$round-$n"), user(n))
      })
      assertEquals(results.count(_.isRight), 30)
      assertEquals(
        results.collect { case Left(rejection) => rejection }.toSet,
        Set[Rejection](Rejection.SeatLimitReached(Plan.Starter, 30))
      )
      assertEquals(licences.usedSeats, 30)
      // Each success reports a distinct seat count, so no two grants shared a seat.
      assertEquals(results.collect { case Right(applied) => applied.usedSeats }.sorted, (1 to 30).toList)
    }
  }

  test("concurrent resends of one request take a single seat and all see the same result") {
    (1 to 20).foreach { round =>
      val licences = SpaceLicences(Plan.Free)
      val id = RequestId(s"resend-$round")
      val results = runTogether((1 to 50).map(_ => () => licences.grant(id, user(1))))
      assertEquals(results.distinct, Seq[Result](Right(Applied(Plan.Free, 1))))
      assertEquals(licences.usedSeats, 1)
    }
  }

  test("grants racing with a downgrade never leave more seats in use than the plan offers") {
    (1 to 20).foreach { round =>
      val licences = SpaceLicences(Plan.Starter)
      (1 to 9).foreach(n => licences.grant(RequestId(s"pre-$round-$n"), user(n)))
      val grants = (10 to 20).map { n => () =>
        licences.grant(RequestId(s"race-$round-$n"), user(n))
      }
      val downgrade = () => licences.changePlan(RequestId(s"down-$round"), Plan.Free)
      runTogether(grants :+ downgrade)
      assert(licences.plan.seatLimit.allows(licences.usedSeats), s"${licences.plan} ${licences.usedSeats}")
    }
  }
