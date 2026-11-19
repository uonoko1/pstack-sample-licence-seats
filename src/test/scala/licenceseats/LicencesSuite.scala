package licenceseats

import java.time.Duration
import java.time.Instant
import java.util.UUID

class LicencesSuite extends munit.FunSuite:

  private def user(n: Int): UserId = UserId(s"user-$n")

  private def users(count: Int): Set[UserId] = (1 to count).map(user).toSet

  private val start = Instant.parse("2026-01-01T00:00:00Z")

  private def day(n: Int): Instant = start.plus(Duration.ofDays(n))

  private def fresh(command: Command, at: Instant = start): Request =
    Request(RequestId(UUID.randomUUID().toString), command, at)

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
    test(s"a downgrade from $from to $to with $holders holders starts a 14-day grace period that rejects grants") {
      val (downgraded, result) = withHolders(from, holders).handle(fresh(Command.ChangePlan(to), day(3)))
      assertEquals(result, Right(()))
      assertEquals(downgraded.plan, to)
      assertEquals(downgraded.holders, users(holders))
      assertEquals(downgraded.standing(day(3)), Standing.InGrace(day(3 + 14)))

      val (_, grant) = downgraded.handle(fresh(Command.Grant(user(holders + 1)), day(4)))

      assertEquals(grant, Left(Rejection.SeatLimitReached(seats)))
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
      assertEquals(downgraded.standing(start), Standing.Normal)

      val (_, nextGrant) = downgraded.handle(fresh(Command.Grant(user(seats + 1))))
      assertEquals(nextGrant, Left(Rejection.SeatLimitReached(seats)))
    }

  test("a grace period ends when revokes bring the holders within the seats") {
    val (inGrace, _) = withHolders(Plan.Starter, 12).handle(fresh(Command.ChangePlan(Plan.Free), day(0)))

    val (stillOver, firstRevoke) = inGrace.handle(fresh(Command.Revoke(user(12)), day(5)))
    assertEquals(firstRevoke, Right(()))
    assertEquals(stillOver.standing(day(5)), Standing.InGrace(day(14)))

    val (fitting, secondRevoke) = stillOver.handle(fresh(Command.Revoke(user(11)), day(6)))
    assertEquals(secondRevoke, Right(()))
    assertEquals(fitting.standing(day(6)), Standing.Normal)
    assertEquals(fitting.standing(day(30)), Standing.Normal)
  }

  test("after a grace period ends, holders who exceed the seats again get a new deadline") {
    val (firstGrace, _) = withHolders(Plan.Standard, 31).handle(fresh(Command.ChangePlan(Plan.Starter), day(0)))
    val (normal, _) = firstGrace.handle(fresh(Command.Revoke(user(31)), day(2)))
    assertEquals(normal.standing(day(2)), Standing.Normal)

    val (secondGrace, result) = normal.handle(fresh(Command.ChangePlan(Plan.Free), day(10)))

    assertEquals(result, Right(()))
    assertEquals(secondGrace.standing(day(10)), Standing.InGrace(day(10 + 14)))
  }

  test("a grace period expires into suspension once 14 days pass with the holders still over") {
    val (inGrace, _) = withHolders(Plan.Starter, 11).handle(fresh(Command.ChangePlan(Plan.Free), day(0)))

    assertEquals(inGrace.standing(day(14).minusNanos(1)), Standing.InGrace(day(14)))
    assertEquals(inGrace.standing(day(14)), Standing.Suspended)
    assertEquals(inGrace.standing(day(100)), Standing.Suspended)

    val (_, grant) = inGrace.handle(fresh(Command.Grant(user(12)), day(15)))
    assertEquals(grant, Left(Rejection.SeatLimitReached(10)))
  }

  test("a revoke that brings the holders within the seats ends a suspension") {
    val (suspended, _) = withHolders(Plan.Starter, 11).handle(fresh(Command.ChangePlan(Plan.Free), day(0)))
    assertEquals(suspended.standing(day(20)), Standing.Suspended)

    val (fitting, revoke) = suspended.handle(fresh(Command.Revoke(user(11)), day(20)))

    assertEquals(revoke, Right(()))
    assertEquals(fitting.standing(day(20)), Standing.Normal)
  }

  test("a further downgrade during the grace period keeps the original deadline") {
    val (inGrace, _) = withHolders(Plan.Standard, 31).handle(fresh(Command.ChangePlan(Plan.Starter), day(0)))

    val (further, result) = inGrace.handle(fresh(Command.ChangePlan(Plan.Free), day(5)))

    assertEquals(result, Right(()))
    assertEquals(further.plan, Plan.Free)
    assertEquals(further.standing(day(5)), Standing.InGrace(day(14)))
    assertEquals(further.standing(day(14)), Standing.Suspended)
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

  test("a downgrade resent much later returns its first result and changes nothing") {
    val downgrade = fresh(Command.ChangePlan(Plan.Free), day(0))
    val (inGrace, first) = withHolders(Plan.Starter, 11).handle(downgrade)
    assertEquals(first, Right(()))
    val (upgraded, _) = inGrace.handle(fresh(Command.ChangePlan(Plan.Starter), day(1)))

    val (after, resent) = upgraded.handle(downgrade.copy(at = day(30)))

    assertEquals(resent, Right(()))
    assertEquals(after.plan, Plan.Starter)
    assertEquals(after.standing(day(30)), Standing.Normal)
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

  test("a request ID reused for a different command is rejected, and the first command still replays") {
    val id = RequestId("reused")
    val (full, first) = withHolders(Plan.Free, 10).handle(Request(id, Command.Grant(user(11)), start))
    assertEquals(first, Left(Rejection.SeatLimitReached(10)))

    val (afterReuse, reused) = full.handle(Request(id, Command.Revoke(user(1)), start))
    assertEquals(reused, Left(Rejection.RequestIdReused))
    assertEquals(afterReuse.holders, users(10))

    val (_, replayed) = afterReuse.handle(Request(id, Command.Grant(user(11)), start))
    assertEquals(replayed, Left(Rejection.SeatLimitReached(10)))
  }
