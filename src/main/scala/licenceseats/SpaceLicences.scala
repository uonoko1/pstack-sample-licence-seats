package licenceseats

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

/** Thread-safe, in-memory licence seats of a single space.
  *
  * Every command carries a request ID. Sending the same command again with the same ID returns
  * the result of the first attempt without applying it twice, however much time has passed.
  *
  * The system clock is never read: the caller passes the current time with every request.
  */
final class SpaceLicences(initialPlan: Plan):
  private val ledger = AtomicReference(SeatLedger.empty(initialPlan))

  def grant(requestId: RequestId, user: UserId, now: Instant): Result =
    submit(requestId, Command.Grant(user), now)

  def revoke(requestId: RequestId, user: UserId, now: Instant): Result =
    submit(requestId, Command.Revoke(user), now)

  def changePlan(requestId: RequestId, plan: Plan, now: Instant): Result =
    submit(requestId, Command.ChangePlan(plan), now)

  def submit(requestId: RequestId, command: Command, now: Instant): Result =
    // Compare-and-set retry: the check and the update are computed from one snapshot and only
    // published if nobody else changed the state in between.
    var result: Result = null
    ledger.updateAndGet { current =>
      val (next, r) = current.handle(requestId, command, now)
      result = r
      next
    }
    result

  def plan: Plan = ledger.get.plan

  /** The standing of the space at `now`: normal, in grace with its deadline, or suspended. */
  def standing(now: Instant): Standing = ledger.get.standing(now)

  def usedSeats: Int = ledger.get.usedSeats
  def holders: Set[UserId] = ledger.get.holders
  def isLicensed(user: UserId): Boolean = ledger.get.holders.contains(user)
