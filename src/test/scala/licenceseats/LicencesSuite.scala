package licenceseats

class LicencesSuite extends munit.FunSuite, Fixtures:

  for (plan, seats) <- List(Plan.Free -> 10, Plan.Starter -> 30) do
    test(s"$plan accepts $seats grants and rejects the next one") {
      val full = withHolders(plan, seats)
      assertEquals(full.holders, users(seats))

      val (after, result) = full.handle(fresh(Command.Grant(user(seats + 1))))

      assertEquals(result, Left(Rejection.SeatLimitReached(seats)))
      assertEquals(after.holders, users(seats))
    }

  test("Standard accepts far more grants than any limited plan") {
    assertEquals(withHolders(Plan.Standard, 1000).holders, users(1000))
  }

  test("a grant is accepted while a seat is free") {
    val (after, result) = withHolders(Plan.Free, 9).handle(fresh(Command.Grant(user(10))))

    assertEquals(result, Right(()))
    assertEquals(after.holders, users(10))
  }

  test("a revoke frees the seat for another user") {
    val (revoked, revokeResult) = withHolders(Plan.Free, 10).handle(fresh(Command.Revoke(user(1))))
    assertEquals(revokeResult, Right(()))

    val (after, grantResult) = revoked.handle(fresh(Command.Grant(user(11))))

    assertEquals(grantResult, Right(()))
    assertEquals(after.holders, users(11) - user(1))
  }

  test("a grant to a user who already holds a licence takes no second seat") {
    val (after, result) = withHolders(Plan.Free, 10).handle(fresh(Command.Grant(user(1))))

    assertEquals(result, Right(()))
    assertEquals(after.holders, users(10))
  }

  test("a revoke from a user who holds no licence changes nothing") {
    val (after, result) = withHolders(Plan.Free, 3).handle(fresh(Command.Revoke(user(4))))

    assertEquals(result, Right(()))
    assertEquals(after.holders, users(3))
  }

  for (from, to, seats) <- List(
      (Plan.Starter, Plan.Free, 10),
      (Plan.Standard, Plan.Free, 10),
      (Plan.Standard, Plan.Starter, 30)
    )
  do
    test(s"a downgrade from $from to $to is accepted when the holders exactly fill $to") {
      val (downgraded, result) = withHolders(from, seats).handle(fresh(Command.ChangePlan(to)))

      assertEquals(result, Right(()))
      assertEquals(downgraded.plan, to)

      val (_, nextGrant) = downgraded.handle(fresh(Command.Grant(user(seats + 1))))
      assertEquals(nextGrant, Left(Rejection.SeatLimitReached(seats)))
    }

  test("an upgrade raises the seat limit") {
    val (upgraded, result) = withHolders(Plan.Free, 10).handle(fresh(Command.ChangePlan(Plan.Starter)))
    assertEquals(result, Right(()))
    assertEquals(upgraded.plan, Plan.Starter)

    val (after, grantResult) = upgraded.handle(fresh(Command.Grant(user(11))))

    assertEquals(grantResult, Right(()))
    assertEquals(after.holders, users(11))
  }

  test("a resent grant that was accepted is accepted again and grants nothing new") {
    val grant = fresh(Command.Grant(user(1)))
    val (granted, first) = Licences.empty(Plan.Free).handle(grant)
    assertEquals(first, Right(()))
    val (revoked, _) = granted.handle(fresh(Command.Revoke(user(1))))

    val (after, resent) = revoked.handle(grant)

    assertEquals(resent, Right(()))
    assertEquals(after.holders, Set.empty[UserId])
  }

  test("a resent grant that was rejected is rejected again, even after a seat is freed") {
    val grant = fresh(Command.Grant(user(11)))
    val (full, first) = withHolders(Plan.Free, 10).handle(grant)
    assertEquals(first, Left(Rejection.SeatLimitReached(10)))
    val (freed, _) = full.handle(fresh(Command.Revoke(user(1))))

    val (after, resent) = freed.handle(grant)

    assertEquals(resent, Left(Rejection.SeatLimitReached(10)))
    assertEquals(after.holders, users(10) - user(1))
  }

  test("a resent revoke that was accepted revokes nothing new") {
    val revoke = fresh(Command.Revoke(user(1)))
    val (revoked, first) = withHolders(Plan.Free, 1).handle(revoke)
    assertEquals(first, Right(()))
    val (regranted, _) = revoked.handle(fresh(Command.Grant(user(1))))

    val (after, resent) = regranted.handle(revoke)

    assertEquals(resent, Right(()))
    assertEquals(after.holders, users(1))
  }

  test("a resend that carries a later time gets the first result") {
    val id = RequestId("resent-later")
    val (full, first) = withHolders(Plan.Free, 10).handle(Request(id, Command.Grant(user(11)), day(0)))
    assertEquals(first, Left(Rejection.SeatLimitReached(10)))
    val (freed, _) = full.handle(fresh(Command.Revoke(user(1)), at = day(1)))

    val (after, resent) = freed.handle(Request(id, Command.Grant(user(11)), day(400)))

    assertEquals(resent, Left(Rejection.SeatLimitReached(10)))
    assertEquals(after.holders, users(10) - user(1))
  }

  test("a request ID reused for a different command is rejected, and the first command still replays") {
    val id = RequestId("reused")
    val (full, first) = withHolders(Plan.Free, 10).handle(Request(id, Command.Grant(user(11)), day(0)))
    assertEquals(first, Left(Rejection.SeatLimitReached(10)))

    val (afterReuse, reused) = full.handle(Request(id, Command.Revoke(user(1)), day(0)))
    assertEquals(reused, Left(Rejection.RequestIdReused))
    assertEquals(afterReuse.holders, users(10))

    val (_, replayed) = afterReuse.handle(Request(id, Command.Grant(user(11)), day(0)))
    assertEquals(replayed, Left(Rejection.SeatLimitReached(10)))
  }
