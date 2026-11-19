package licenceseats

import java.time.{Duration, Instant}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

class SpaceLicencesSuite extends munit.FunSuite:

  private var requestCounter = 0
  private def freshId(): RequestId =
    requestCounter += 1
    RequestId(s"req-$requestCounter")

  private val t0 = Instant.parse("2026-01-01T00:00:00Z")
  private def day(n: Int): Instant = t0.plus(Duration.ofDays(n))
  private val graceDeadline = day(14)

  private def user(n: Int): UserId = UserId(s"user-$n")

  private def fill(licences: SpaceLicences, count: Int): Unit =
    (1 to count).foreach(n => assert(licences.grant(freshId(), user(n), t0).isRight, s"grant $n"))

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
    assertEquals(licences.grant(freshId(), user(1), t0), Right(Applied(Plan.Free, 1, Standing.Normal)))
    assert(licences.isLicensed(user(1)))
    assertEquals(licences.revoke(freshId(), user(1), t0), Right(Applied(Plan.Free, 0, Standing.Normal)))
    assert(!licences.isLicensed(user(1)))
    assertEquals(licences.usedSeats, 0)
  }

  test("a grant beyond the seat limit is rejected") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    assertEquals(
      licences.grant(freshId(), user(11), t0),
      Left(Rejection.SeatLimitReached(Plan.Free, 10))
    )
    assertEquals(licences.usedSeats, 10)
    assert(!licences.isLicensed(user(11)))
  }

  test("a seat freed by a revoke can be granted again") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    assert(licences.revoke(freshId(), user(1), t0).isRight)
    assertEquals(licences.grant(freshId(), user(11), t0), Right(Applied(Plan.Free, 10, Standing.Normal)))
  }

  test("the Standard plan has no seat limit") {
    val licences = SpaceLicences(Plan.Standard)
    fill(licences, 500)
    assertEquals(licences.usedSeats, 500)
  }

  test("granting to a user who already holds a licence is rejected and takes no seat") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 1)
    assertEquals(licences.grant(freshId(), user(1), t0), Left(Rejection.AlreadyLicensed(user(1))))
    assertEquals(licences.usedSeats, 1)
  }

  test("revoking from a user without a licence is rejected") {
    val licences = SpaceLicences(Plan.Free)
    assertEquals(licences.revoke(freshId(), user(1), t0), Left(Rejection.NotLicensed(user(1))))
  }

  test("upgrading raises the seat limit") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    assertEquals(licences.changePlan(freshId(), Plan.Starter, t0), Right(Applied(Plan.Starter, 10, Standing.Normal)))
    assertEquals(licences.grant(freshId(), user(11), t0), Right(Applied(Plan.Starter, 11, Standing.Normal)))
  }

  /** A Free space holding `holders` licences, downgraded from Starter at `t0`. */
  private def overFreeLimit(holders: Int): SpaceLicences =
    val licences = SpaceLicences(Plan.Starter)
    fill(licences, holders)
    assert(licences.changePlan(freshId(), Plan.Free, t0).isRight)
    licences

  test("a downgrade below the seats in use is accepted and starts a 14-day grace period") {
    val licences = SpaceLicences(Plan.Starter)
    fill(licences, 11)
    assertEquals(licences.standing(t0), Standing.Normal)
    assertEquals(
      licences.changePlan(freshId(), Plan.Free, t0),
      Right(Applied(Plan.Free, 11, Standing.InGrace(graceDeadline)))
    )
    assertEquals(licences.plan, Plan.Free)
    assertEquals(licences.usedSeats, 11)
    assertEquals(licences.standing(t0), Standing.InGrace(graceDeadline))
    assertEquals(licences.standing(graceDeadline.minusMillis(1)), Standing.InGrace(graceDeadline))
  }

  test("a downgrade from the unlimited plan below the seats in use starts a grace period") {
    val licences = SpaceLicences(Plan.Standard)
    fill(licences, 31)
    assertEquals(
      licences.changePlan(freshId(), Plan.Starter, day(3)),
      Right(Applied(Plan.Starter, 31, Standing.InGrace(day(17))))
    )
  }

  test("grants are rejected while the space is over its limit, in grace and when suspended") {
    val licences = overFreeLimit(12)
    val rejected = Left(Rejection.SeatLimitReached(Plan.Free, 10))
    assertEquals(licences.grant(freshId(), user(100), day(1)), rejected)
    assertEquals(licences.grant(freshId(), user(100), day(20)), rejected)
    assertEquals(licences.usedSeats, 12)
  }

  test("a revoke during grace that leaves the space over its limit keeps the deadline") {
    val licences = overFreeLimit(12)
    assertEquals(
      licences.revoke(freshId(), user(1), day(5)),
      Right(Applied(Plan.Free, 11, Standing.InGrace(graceDeadline)))
    )
    assertEquals(licences.standing(day(5)), Standing.InGrace(graceDeadline))
  }

  test("a revoke that brings the holders within the limit ends the grace period") {
    val licences = overFreeLimit(11)
    assertEquals(
      licences.revoke(freshId(), user(1), day(5)),
      Right(Applied(Plan.Free, 10, Standing.Normal))
    )
    assertEquals(licences.standing(day(5)), Standing.Normal)
    // The old deadline is gone for good, not merely not yet reached.
    assertEquals(licences.standing(day(30)), Standing.Normal)
    assert(licences.revoke(freshId(), user(2), day(30)).isRight)
    assertEquals(licences.grant(freshId(), user(100), day(30)), Right(Applied(Plan.Free, 10, Standing.Normal)))
  }

  test("an upgrade that fits the holders ends the grace period") {
    val licences = overFreeLimit(11)
    assertEquals(
      licences.changePlan(freshId(), Plan.Starter, day(5)),
      Right(Applied(Plan.Starter, 11, Standing.Normal))
    )
    assertEquals(licences.standing(day(30)), Standing.Normal)
  }

  test("going over the limit again after a grace period ended starts a fresh 14 days") {
    val licences = overFreeLimit(11)
    assert(licences.changePlan(freshId(), Plan.Starter, day(5)).isRight)
    assertEquals(
      licences.changePlan(freshId(), Plan.Free, day(10)),
      Right(Applied(Plan.Free, 11, Standing.InGrace(day(24))))
    )
  }

  test("a further downgrade during grace is accepted and keeps the first deadline") {
    val licences = SpaceLicences(Plan.Standard)
    fill(licences, 31)
    assert(licences.changePlan(freshId(), Plan.Starter, t0).isRight)
    assertEquals(
      licences.changePlan(freshId(), Plan.Free, day(10)),
      Right(Applied(Plan.Free, 31, Standing.InGrace(graceDeadline)))
    )
    assertEquals(licences.standing(day(13)), Standing.InGrace(graceDeadline))
    assertEquals(licences.standing(day(15)), Standing.Suspended(graceDeadline))
  }

  test("the space is suspended once the grace period expires with the overage unresolved") {
    val licences = overFreeLimit(11)
    assertEquals(licences.standing(graceDeadline.minusMillis(1)), Standing.InGrace(graceDeadline))
    assertEquals(licences.standing(graceDeadline), Standing.Suspended(graceDeadline))
    assertEquals(licences.standing(day(100)), Standing.Suspended(graceDeadline))
  }

  test("a suspended space rejects a change to a plan the holders still exceed") {
    val licences = SpaceLicences(Plan.Standard)
    fill(licences, 31)
    assert(licences.changePlan(freshId(), Plan.Starter, t0).isRight)
    assertEquals(
      licences.changePlan(freshId(), Plan.Free, day(14)),
      Left(Rejection.SeatsInUseExceedPlan(Plan.Free, 10, 31))
    )
    assertEquals(licences.plan, Plan.Starter)
    assertEquals(licences.standing(day(14)), Standing.Suspended(graceDeadline))
  }

  test("a suspended space rejects even an upgrade that the holders still exceed") {
    val licences = SpaceLicences(Plan.Standard)
    fill(licences, 31)
    assert(licences.changePlan(freshId(), Plan.Free, t0).isRight)
    assertEquals(
      licences.changePlan(freshId(), Plan.Starter, day(20)),
      Left(Rejection.SeatsInUseExceedPlan(Plan.Starter, 30, 31))
    )
    assertEquals(licences.plan, Plan.Free)
  }

  test("a suspended space accepts a change to a plan the holders fit, which lifts the suspension") {
    val licences = overFreeLimit(11)
    assertEquals(
      licences.changePlan(freshId(), Plan.Starter, day(20)),
      Right(Applied(Plan.Starter, 11, Standing.Normal))
    )
    assertEquals(licences.standing(day(20)), Standing.Normal)
    assertEquals(licences.grant(freshId(), user(100), day(20)), Right(Applied(Plan.Starter, 12, Standing.Normal)))
  }

  test("revokes are accepted while suspended, and the suspension lifts once the holders fit") {
    val licences = overFreeLimit(12)
    assertEquals(
      licences.revoke(freshId(), user(1), day(20)),
      Right(Applied(Plan.Free, 11, Standing.Suspended(graceDeadline)))
    )
    assertEquals(
      licences.revoke(freshId(), user(2), day(21)),
      Right(Applied(Plan.Free, 10, Standing.Normal))
    )
    assertEquals(licences.standing(day(21)), Standing.Normal)
    // Back to normal, a downgrade below the seats in use starts a new grace period.
    assert(licences.changePlan(freshId(), Plan.Starter, day(21)).isRight)
    assert(licences.grant(freshId(), user(100), day(21)).isRight)
    assertEquals(
      licences.changePlan(freshId(), Plan.Free, day(22)),
      Right(Applied(Plan.Free, 11, Standing.InGrace(day(36))))
    )
  }

  test("resending a downgrade after its grace period expired returns the original result") {
    val licences = SpaceLicences(Plan.Starter)
    fill(licences, 11)
    val id = freshId()
    val first = licences.changePlan(id, Plan.Free, t0)
    assertEquals(first, Right(Applied(Plan.Free, 11, Standing.InGrace(graceDeadline))))
    assertEquals(licences.changePlan(id, Plan.Free, day(30)), first)
    // The resend neither restarted the grace period nor lifted the suspension.
    assertEquals(licences.standing(day(30)), Standing.Suspended(graceDeadline))
  }

  test("resending a grant rejected during grace returns that rejection after the overage is resolved") {
    val licences = overFreeLimit(11)
    val id = freshId()
    val first = licences.grant(id, user(100), day(1))
    assertEquals(first, Left(Rejection.SeatLimitReached(Plan.Free, 10)))
    assert(licences.revoke(freshId(), user(1), day(2)).isRight)
    assert(licences.revoke(freshId(), user(2), day(2)).isRight)
    assertEquals(licences.grant(id, user(100), day(3)), first)
    assertEquals(licences.usedSeats, 9)
  }

  test("a downgrade is accepted when the seats in use exactly fill the target plan") {
    val licences = SpaceLicences(Plan.Starter)
    fill(licences, 10)
    assertEquals(licences.changePlan(freshId(), Plan.Free, t0), Right(Applied(Plan.Free, 10, Standing.Normal)))
    assertEquals(licences.grant(freshId(), user(11), t0), Left(Rejection.SeatLimitReached(Plan.Free, 10)))
  }

  test("resending a grant returns the same result and does not take a second seat") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    val first = licences.grant(id, user(1), t0)
    assertEquals(first, Right(Applied(Plan.Free, 1, Standing.Normal)))
    // Without replay the resend would be rejected as AlreadyLicensed.
    assertEquals(licences.grant(id, user(1), t0), first)
    assertEquals(licences.usedSeats, 1)
  }

  test("a resend returns the original result even after the state has moved on") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    val first = licences.grant(id, user(99), t0)
    assertEquals(first, Right(Applied(Plan.Free, 1, Standing.Normal)))
    fill(licences, 5)
    assertEquals(licences.grant(id, user(99), t0), first)
    assertEquals(licences.usedSeats, 6)
  }

  test("resending a revoke returns the original success and does not undo a later grant") {
    val licences = SpaceLicences(Plan.Free)
    assert(licences.grant(freshId(), user(1), t0).isRight)
    val id = freshId()
    val first = licences.revoke(id, user(1), t0)
    assertEquals(first, Right(Applied(Plan.Free, 0, Standing.Normal)))
    assert(licences.grant(freshId(), user(1), t0).isRight)
    assertEquals(licences.revoke(id, user(1), t0), first)
    assert(licences.isLicensed(user(1)), "the resend must not revoke the newer licence")
  }

  test("resending a rejected grant returns the same rejection even after a seat is freed") {
    val licences = SpaceLicences(Plan.Free)
    fill(licences, 10)
    val id = freshId()
    val first = licences.grant(id, user(11), t0)
    assertEquals(first, Left(Rejection.SeatLimitReached(Plan.Free, 10)))
    assert(licences.revoke(freshId(), user(1), t0).isRight)
    assertEquals(licences.grant(id, user(11), t0), first)
    assertEquals(licences.usedSeats, 9)
  }

  test("resending a plan change returns the same result and does not reapply it") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    val first = licences.changePlan(id, Plan.Starter, t0)
    assert(licences.changePlan(freshId(), Plan.Standard, t0).isRight)
    assertEquals(licences.changePlan(id, Plan.Starter, t0), first)
    assertEquals(licences.plan, Plan.Standard)
  }

  test("reusing a request ID for a different command is rejected and changes nothing") {
    val licences = SpaceLicences(Plan.Free)
    val id = freshId()
    assert(licences.grant(id, user(1), t0).isRight)
    assertEquals(
      licences.grant(id, user(2), t0),
      Left(Rejection.RequestIdReused(id, Command.Grant(user(1))))
    )
    assertEquals(licences.holders, Set(user(1)))
    // The original request is still replayable afterwards.
    assertEquals(licences.grant(id, user(1), t0), Right(Applied(Plan.Free, 1, Standing.Normal)))
  }

  test("concurrent grants never exceed the seat limit") {
    (1 to 20).foreach { round =>
      val licences = SpaceLicences(Plan.Starter)
      val results = runTogether((1 to 100).map { n => () =>
        licences.grant(RequestId(s"round-$round-$n"), user(n), t0)
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
      val results = runTogether((1 to 50).map(_ => () => licences.grant(id, user(1), t0)))
      assertEquals(results.distinct, Seq[Result](Right(Applied(Plan.Free, 1, Standing.Normal))))
      assertEquals(licences.usedSeats, 1)
    }
  }

  test("grants racing with a downgrade never take a seat beyond the limit of their plan") {
    (1 to 20).foreach { round =>
      val licences = SpaceLicences(Plan.Starter)
      (1 to 9).foreach(n => licences.grant(RequestId(s"pre-$round-$n"), user(n), t0))
      val grants = (10 to 20).map { n => () =>
        licences.grant(RequestId(s"race-$round-$n"), user(n), t0)
      }
      val downgrade = () => licences.changePlan(RequestId(s"down-$round"), Plan.Free, t0)
      val grantResults = runTogether(grants :+ downgrade).init
      // Grants that won the race against the downgrade may leave the space over the Free limit
      // (in grace), but no grant is ever applied on top of a full or exceeded plan.
      grantResults.collect { case Right(applied) => applied }.foreach { applied =>
        assert(applied.plan.seatLimit.allows(applied.usedSeats), applied.toString)
      }
      assertEquals(licences.plan, Plan.Free)
      val expected =
        if licences.usedSeats > 10 then Standing.InGrace(graceDeadline) else Standing.Normal
      assertEquals(licences.standing(t0), expected)
    }
  }

  test("concurrent grants during grace are all rejected") {
    val licences = overFreeLimit(11)
    val results = runTogether((1 to 50).map { n => () =>
      licences.grant(RequestId(s"grace-grant-$n"), user(100 + n), day(1))
    })
    assertEquals(results.toSet, Set[Result](Left(Rejection.SeatLimitReached(Plan.Free, 10))))
    assertEquals(licences.usedSeats, 11)
  }
