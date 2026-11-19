package licenceseats

import java.util.UUID

class LicencesSuite extends munit.FunSuite:

  private def user(n: Int): UserId = UserId(s"user-$n")

  private def users(count: Int): Set[UserId] = (1 to count).map(user).toSet

  private def fresh(command: Command): Request =
    Request(RequestId(UUID.randomUUID().toString), command)

  private def withHolders(plan: Plan, count: Int): Licences =
    users(count).foldLeft(Licences.empty(plan)) { (licences, holder) =>
      licences.handle(fresh(Command.Grant(holder)))._1
    }

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

  for (from, holders, to, seats) <- List(
      (Plan.Starter, 11, Plan.Free, 10),
      (Plan.Standard, 11, Plan.Free, 10),
      (Plan.Standard, 31, Plan.Starter, 30)
    )
  do
    test(s"a downgrade from $from to $to is rejected with $holders holders") {
      val (after, result) = withHolders(from, holders).handle(fresh(Command.ChangePlan(to)))

      assertEquals(result, Left(Rejection.TooManyHolders(seats, holders)))
      assertEquals(after.plan, from)
      assertEquals(after.holders, users(holders))
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
