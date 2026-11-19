package licenceseats

class GracePeriodSuite extends munit.FunSuite, Fixtures:

  /** Free has 10 seats. The downgrade is at day 0, so the 14 days end at day 14. */
  private def overFree(holders: Int): Licences =
    withHolders(Plan.Starter, holders).handle(fresh(Command.ChangePlan(Plan.Free), at = day(0)))._1

  test("a space whose holders fit its plan is normal at any time") {
    val full = withHolders(Plan.Free, 10)
    val (exactlyFilled, _) = withHolders(Plan.Starter, 10).handle(fresh(Command.ChangePlan(Plan.Free), at = day(0)))

    assertEquals(full.standingAt(day(0)), Standing.Normal)
    assertEquals(full.standingAt(day(1000)), Standing.Normal)
    assertEquals(exactlyFilled.standingAt(day(0)), Standing.Normal)
    assertEquals(exactlyFilled.standingAt(day(1000)), Standing.Normal)
  }

  for (from, holders, to) <- List(
      (Plan.Starter, 11, Plan.Free),
      (Plan.Standard, 11, Plan.Free),
      (Plan.Standard, 31, Plan.Starter)
    )
  do
    test(s"a downgrade from $from to $to with $holders holders is accepted and starts a 14-day grace period") {
      val (downgraded, result) = withHolders(from, holders).handle(fresh(Command.ChangePlan(to), at = day(3)))

      assertEquals(result, Right(()))
      assertEquals(downgraded.plan, to)
      assertEquals(downgraded.holders, users(holders))
      assertEquals(downgraded.standingAt(day(3)), Standing.InGrace(day(17)))
    }

  test("during the grace period a grant is rejected, for a new user and for a holder") {
    val inGrace = overFree(holders = 11)

    val (afterNewUser, newUser) = inGrace.handle(fresh(Command.Grant(user(12)), at = day(1)))
    val (afterHolder, holder) = inGrace.handle(fresh(Command.Grant(user(1)), at = day(1)))

    assertEquals(newUser, Left(Rejection.SeatLimitReached(10)))
    assertEquals(holder, Left(Rejection.SeatLimitReached(10)))
    assertEquals(afterNewUser.holders, users(11))
    assertEquals(afterHolder.holders, users(11))
  }

  test("a revoke that leaves the holders over the limit is accepted and keeps the deadline") {
    val (revoked, result) = overFree(holders = 12).handle(fresh(Command.Revoke(user(12)), at = day(5)))

    assertEquals(result, Right(()))
    assertEquals(revoked.holders, users(11))
    assertEquals(revoked.standingAt(day(5)), Standing.InGrace(day(14)))
  }

  test("the grace period ends when a revoke makes the holders fit, and the space grants again") {
    val (fitting, revokeResult) = overFree(holders = 11).handle(fresh(Command.Revoke(user(11)), at = day(5)))

    assertEquals(revokeResult, Right(()))
    assertEquals(fitting.standingAt(day(5)), Standing.Normal)
    assertEquals(fitting.standingAt(day(14)), Standing.Normal)

    val (freed, _) = fitting.handle(fresh(Command.Revoke(user(10)), at = day(20)))
    val (after, grantResult) = freed.handle(fresh(Command.Grant(user(12)), at = day(20)))

    assertEquals(grantResult, Right(()))
    assertEquals(after.holders, users(9) + user(12))
    assertEquals(after.standingAt(day(20)), Standing.Normal)
  }

  test("the grace period ends with a change to a plan that the holders fit") {
    val (upgraded, result) = overFree(holders = 11).handle(fresh(Command.ChangePlan(Plan.Starter), at = day(5)))

    assertEquals(result, Right(()))
    assertEquals(upgraded.plan, Plan.Starter)
    assertEquals(upgraded.standingAt(day(5)), Standing.Normal)
    assertEquals(upgraded.standingAt(day(14)), Standing.Normal)
  }

  test("a further downgrade that the holders exceed is accepted and keeps the original deadline") {
    val (onStarter, _) = withHolders(Plan.Standard, 31).handle(fresh(Command.ChangePlan(Plan.Starter), at = day(0)))

    val (onFree, result) = onStarter.handle(fresh(Command.ChangePlan(Plan.Free), at = day(5)))

    assertEquals(result, Right(()))
    assertEquals(onFree.plan, Plan.Free)
    assertEquals(onFree.standingAt(day(5)), Standing.InGrace(day(14)))
  }

  test("a grace period that starts after an earlier one ended gets its own deadline") {
    val (upgraded, _) = overFree(holders = 11).handle(fresh(Command.ChangePlan(Plan.Starter), at = day(5)))

    val (downgradedAgain, result) = upgraded.handle(fresh(Command.ChangePlan(Plan.Free), at = day(20)))

    assertEquals(result, Right(()))
    assertEquals(downgradedAgain.standingAt(day(20)), Standing.InGrace(day(34)))
  }

  test("a grace period whose holders still exceed the limit after 14 days expires into suspension") {
    val over = overFree(holders = 11)

    assertEquals(over.standingAt(day(14).minusSeconds(1)), Standing.InGrace(day(14)))
    assertEquals(over.standingAt(day(14)), Standing.Suspended)
    assertEquals(over.standingAt(day(100)), Standing.Suspended)
  }

  test("a suspended space rejects a grant, and accepts a revoke that leaves it suspended") {
    val over = overFree(holders = 12)

    val (afterGrant, grantResult) = over.handle(fresh(Command.Grant(user(13)), at = day(14)))
    val (revoked, revokeResult) = over.handle(fresh(Command.Revoke(user(12)), at = day(14)))

    assertEquals(grantResult, Left(Rejection.SeatLimitReached(10)))
    assertEquals(afterGrant.holders, users(12))
    assertEquals(revokeResult, Right(()))
    assertEquals(revoked.holders, users(11))
    assertEquals(revoked.standingAt(day(14)), Standing.Suspended)
  }

  test("a suspended space is normal again once a revoke makes the holders fit") {
    val (fitting, result) = overFree(holders = 11).handle(fresh(Command.Revoke(user(11)), at = day(20)))

    assertEquals(result, Right(()))
    assertEquals(fitting.standingAt(day(20)), Standing.Normal)
  }
