package licenceseats

class SpaceSuite extends munit.FunSuite:

  private val freeSeats    = 10
  private val starterSeats = 30

  private def user(n: Int): UserId = UserId(s"user-$n")

  private def run(space: Space, commands: Seq[Command], prefix: String): (Space, Seq[Result]) =
    commands.zipWithIndex.foldLeft((space, Vector.empty[Result])) {
      case ((current, results), (command, n)) =>
        val (next, result) = current.handle(Request(RequestId(s"$prefix-$n"), command))
        (next, results :+ result)
    }

  private def withHolders(plan: Plan, count: Int): Space =
    val (space, results) =
      run(Space.empty(Plan.Standard), (1 to count).map(n => Command.Grant(user(n))), "seed")
    assert(results.forall(_.isRight))
    val (planned, changed) = space.handle(Request(RequestId("seed-plan"), Command.ChangePlan(plan)))
    assertEquals(changed, Right(()))
    planned

  private def assertUnchanged(after: Space, before: Space): Unit =
    assertEquals(after.plan, before.plan)
    assertEquals(after.holders, before.holders)

  test("a new space has the given plan and no holders") {
    val space = Space.empty(Plan.Free)
    assertEquals(space.plan, Plan.Free)
    assertEquals(space.holders, Set.empty[UserId])
  }

  test("a grant makes the user a holder and a revoke removes them") {
    val (granted, grant) = Space.empty(Plan.Free).handle(Request(RequestId("a"), Command.Grant(user(1))))
    assertEquals(grant, Right(()))
    assertEquals(granted.holders, Set(user(1)))

    val (revoked, revoke) = granted.handle(Request(RequestId("b"), Command.Revoke(user(1))))
    assertEquals(revoke, Right(()))
    assertEquals(revoked.holders, Set.empty[UserId])
  }

  List(Plan.Free -> freeSeats, Plan.Starter -> starterSeats).foreach { (plan, seats) =>
    test(s"$plan accepts grants up to $seats seats and rejects the next one") {
      val full = withHolders(plan, seats)
      assertEquals(full.holders.size, seats)

      val (after, result) = full.handle(Request(RequestId("over"), Command.Grant(user(seats + 1))))
      assertEquals(result, Left(Rejection.SeatLimitReached(seats)))
      assertUnchanged(after, full)
    }
  }

  test("Standard accepts more holders than any limited plan") {
    val space = withHolders(Plan.Standard, starterSeats + 1)
    assertEquals(space.holders.size, starterSeats + 1)
  }

  test("a grant to a user who already holds a licence is accepted and takes no extra seat") {
    val full            = withHolders(Plan.Free, freeSeats)
    val (after, result) = full.handle(Request(RequestId("again"), Command.Grant(user(1))))
    assertEquals(result, Right(()))
    assertUnchanged(after, full)
  }

  test("a revoke frees a seat for another grant") {
    val full = withHolders(Plan.Free, freeSeats)
    val (after, results) =
      run(full, Seq(Command.Revoke(user(1)), Command.Grant(user(freeSeats + 1))), "swap")
    assertEquals(results, Seq(Right(()), Right(())))
    assertEquals(after.holders, (2 to freeSeats + 1).map(user).toSet)
  }

  test("a revoke from a user without a licence is accepted and changes nothing") {
    val space           = withHolders(Plan.Free, 1)
    val (after, result) = space.handle(Request(RequestId("absent"), Command.Revoke(user(2))))
    assertEquals(result, Right(()))
    assertUnchanged(after, space)
  }

  test("a downgrade is rejected when holders exceed the target limit") {
    val space           = withHolders(Plan.Standard, starterSeats + 1)
    val (after, result) = space.handle(Request(RequestId("down"), Command.ChangePlan(Plan.Starter)))
    assertEquals(result, Left(Rejection.TooManyHolders(starterSeats + 1, starterSeats)))
    assertUnchanged(after, space)

    val (_, toFree) = space.handle(Request(RequestId("down-free"), Command.ChangePlan(Plan.Free)))
    assertEquals(toFree, Left(Rejection.TooManyHolders(starterSeats + 1, freeSeats)))
  }

  test("a downgrade is accepted when holders exactly fill the target limit") {
    val space           = withHolders(Plan.Starter, freeSeats)
    val (after, result) = space.handle(Request(RequestId("down"), Command.ChangePlan(Plan.Free)))
    assertEquals(result, Right(()))
    assertEquals(after.plan, Plan.Free)
    assertEquals(after.holders, space.holders)
  }

  test("an upgrade is accepted and lifts the seat limit") {
    val full = withHolders(Plan.Free, freeSeats)
    val (after, results) =
      run(full, Seq(Command.ChangePlan(Plan.Starter), Command.Grant(user(freeSeats + 1))), "up")
    assertEquals(results, Seq(Right(()), Right(())))
    assertEquals(after.plan, Plan.Starter)
    assertEquals(after.holders.size, freeSeats + 1)
  }

  test("resending an accepted request returns the first result with no second effect") {
    val grant               = Request(RequestId("grant"), Command.Grant(user(1)))
    val (granted, first)    = Space.empty(Plan.Free).handle(grant)
    val (revoked, _)        = granted.handle(Request(RequestId("revoke"), Command.Revoke(user(1))))
    val (afterResend, again) = revoked.handle(grant)
    assertEquals(first, Right(()))
    assertEquals(again, first)
    assertEquals(afterResend.holders, Set.empty[UserId])
  }

  test("resending a rejected request returns the first rejection even once it would succeed") {
    val full                  = withHolders(Plan.Free, freeSeats)
    val grant                 = Request(RequestId("grant"), Command.Grant(user(freeSeats + 1)))
    val (rejected, first)     = full.handle(grant)
    val (freed, _)            = rejected.handle(Request(RequestId("revoke"), Command.Revoke(user(1))))
    val (afterResend, again)  = freed.handle(grant)
    assertEquals(first, Left(Rejection.SeatLimitReached(freeSeats)))
    assertEquals(again, first)
    assertUnchanged(afterResend, freed)
  }

  test("resending a rejected downgrade returns the first rejection with no plan change") {
    val space             = withHolders(Plan.Starter, freeSeats + 1)
    val downgrade         = Request(RequestId("down"), Command.ChangePlan(Plan.Free))
    val (rejected, first) = space.handle(downgrade)
    val (freed, _)        = rejected.handle(Request(RequestId("revoke"), Command.Revoke(user(1))))
    val (after, again)    = freed.handle(downgrade)
    assertEquals(first, Left(Rejection.TooManyHolders(freeSeats + 1, freeSeats)))
    assertEquals(again, first)
    assertEquals(after.plan, Plan.Starter)
  }

  test("a request ID answers with its first result even when resent with another command") {
    val id               = RequestId("same")
    val (granted, first) = Space.empty(Plan.Free).handle(Request(id, Command.Grant(user(1))))
    val (after, again)   = granted.handle(Request(id, Command.ChangePlan(Plan.Standard)))
    assertEquals(again, first)
    assertUnchanged(after, granted)
  }
