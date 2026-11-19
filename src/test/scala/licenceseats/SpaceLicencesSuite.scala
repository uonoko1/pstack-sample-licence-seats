package licenceseats

import java.util.UUID

class SpaceLicencesSuite extends munit.FunSuite:
  private val accepted: Either[Rejection, Unit] = Right(())

  private def user(n: Int): UserId = UserId(s"user-$n")
  private def users(range: Range): Set[UserId] = range.map(user).toSet
  private def freshId(): RequestId = RequestId(UUID.randomUUID().toString)
  private def grant(u: UserId, id: RequestId = freshId()): Request = Request(id, Command.Grant(u))
  private def revoke(u: UserId, id: RequestId = freshId()): Request = Request(id, Command.Revoke(u))
  private def changePlan(to: Plan, id: RequestId = freshId()): Request = Request(id, Command.ChangePlan(to))

  private def run(from: SpaceLicences, requests: Request*): (SpaceLicences, Seq[Either[Rejection, Unit]]) =
    requests.foldLeft((from, Vector.empty[Either[Rejection, Unit]])) { case ((state, results), request) =>
      val (next, result) = state.handle(request)
      (next, results :+ result)
    }

  /** `plan` with users 1 to `count` holding licences. */
  private def holding(plan: Plan, count: Int): SpaceLicences =
    val (state, results) = run(SpaceLicences.empty(plan), (1 to count).map(n => grant(user(n)))*)
    assert(results.forall(_ == accepted), results)
    state

  // Rule 1: a grant that would exceed the seat limit is rejected.

  test("Free grants 10 licences and rejects the 11th"):
    val (state, results) = run(SpaceLicences.empty(Plan.Free), (1 to 11).map(n => grant(user(n)))*)
    assertEquals(results, Vector.fill(10)(accepted) :+ Left(Rejection.SeatLimitReached(10)))
    assertEquals(state.holders, users(1 to 10))

  test("Starter grants 30 licences and rejects the 31st"):
    val (state, results) = run(SpaceLicences.empty(Plan.Starter), (1 to 31).map(n => grant(user(n)))*)
    assertEquals(results, Vector.fill(30)(accepted) :+ Left(Rejection.SeatLimitReached(30)))
    assertEquals(state.holders, users(1 to 30))

  test("Standard grants without a seat limit"):
    val (state, results) = run(SpaceLicences.empty(Plan.Standard), (1 to 1000).map(n => grant(user(n)))*)
    assertEquals(results, Vector.fill(1000)(accepted))
    assertEquals(state.holders, users(1 to 1000))

  test("a revoke frees a seat for the next grant"):
    val (state, results) = run(holding(Plan.Free, 10), revoke(user(1)), grant(user(11)))
    assertEquals(results, Vector(accepted, accepted))
    assertEquals(state.holders, users(2 to 11))

  test("granting to a current holder at the limit uses no extra seat"):
    val (state, results) = run(holding(Plan.Free, 10), grant(user(1)))
    assertEquals(results, Vector(accepted))
    assertEquals(state.holders, users(1 to 10))

  test("revoking from a user without a licence changes nothing"):
    val (state, results) = run(holding(Plan.Free, 3), revoke(user(4)))
    assertEquals(results, Vector(accepted))
    assertEquals(state.holders, users(1 to 3))

  // Rule 2: a downgrade below the current number of holders is rejected.

  test("a downgrade to a limit below the holder count is rejected and keeps the plan"):
    val (state, results) = run(holding(Plan.Starter, 11), changePlan(Plan.Free))
    assertEquals(results, Vector(Left(Rejection.TooManyHolders(seats = 10, holders = 11))))
    assertEquals(state.plan, Plan.Starter)
    assertEquals(state.holders, users(1 to 11))

  test("a downgrade to a limit equal to the holder count is accepted and then enforced"):
    val (state, results) = run(holding(Plan.Starter, 10), changePlan(Plan.Free), grant(user(11)))
    assertEquals(results, Vector(accepted, Left(Rejection.SeatLimitReached(10))))
    assertEquals(state.plan, Plan.Free)
    assertEquals(state.holders, users(1 to 10))

  test("a downgrade from Standard is checked against the target limit"):
    val (state, results) = run(holding(Plan.Standard, 31), changePlan(Plan.Starter))
    assertEquals(results, Vector(Left(Rejection.TooManyHolders(seats = 30, holders = 31))))
    assertEquals(state.plan, Plan.Standard)

  test("an upgrade lifts the seat limit"):
    val (state, results) = run(holding(Plan.Free, 10), changePlan(Plan.Standard), grant(user(11)))
    assertEquals(results, Vector(accepted, accepted))
    assertEquals(state.plan, Plan.Standard)
    assertEquals(state.holders, users(1 to 11))

  // Rule 3: a resend with the same request ID returns the first result, with no second effect.

  test("resending an accepted grant replays acceptance without granting again"):
    val id = freshId()
    val (state, results) = run(SpaceLicences.empty(Plan.Free), grant(user(1), id), revoke(user(1)), grant(user(1), id))
    assertEquals(results, Vector(accepted, accepted, accepted))
    assertEquals(state.holders, Set.empty[UserId])

  test("resending a rejected grant replays the rejection after a seat frees up"):
    val id = freshId()
    val (state, results) = run(holding(Plan.Free, 10), grant(user(11), id), revoke(user(1)), grant(user(11), id))
    assertEquals(results, Vector(Left(Rejection.SeatLimitReached(10)), accepted, Left(Rejection.SeatLimitReached(10))))
    assertEquals(state.holders, users(2 to 10))

  test("resending an accepted revoke replays acceptance without revoking again"):
    val id = freshId()
    val (state, results) = run(holding(Plan.Free, 1), revoke(user(1), id), grant(user(1)), revoke(user(1), id))
    assertEquals(results, Vector(accepted, accepted, accepted))
    assertEquals(state.holders, users(1 to 1))

  test("resending a rejected downgrade replays the rejection after holders drop"):
    val id = freshId()
    val (state, results) = run(holding(Plan.Starter, 11), changePlan(Plan.Free, id), revoke(user(1)), changePlan(Plan.Free, id))
    val rejected = Left(Rejection.TooManyHolders(seats = 10, holders = 11))
    assertEquals(results, Vector(rejected, accepted, rejected))
    assertEquals(state.plan, Plan.Starter)

  test("reusing a request ID for a different command is rejected without effect"):
    val id = freshId()
    val (state, results) = run(SpaceLicences.empty(Plan.Free), grant(user(1), id), revoke(user(1), id), grant(user(1), id))
    assertEquals(results, Vector(accepted, Left(Rejection.RequestIdReused), accepted))
    assertEquals(state.holders, users(1 to 1))
