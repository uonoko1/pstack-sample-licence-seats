package licenceseats

import java.util.concurrent.atomic.AtomicReference

/** Thread-safe, in-memory licence seats of a single space.
  *
  * Every command carries a request ID. Sending the same command again with the same ID returns
  * the result of the first attempt without applying it twice.
  */
final class SpaceLicences(initialPlan: Plan):
  private val ledger = AtomicReference(SeatLedger.empty(initialPlan))

  def grant(requestId: RequestId, user: UserId): Result = submit(requestId, Command.Grant(user))

  def revoke(requestId: RequestId, user: UserId): Result = submit(requestId, Command.Revoke(user))

  def changePlan(requestId: RequestId, plan: Plan): Result =
    submit(requestId, Command.ChangePlan(plan))

  def submit(requestId: RequestId, command: Command): Result =
    // Compare-and-set retry: the check and the update are computed from one snapshot and only
    // published if nobody else changed the state in between.
    var result: Result = null
    ledger.updateAndGet { current =>
      val (next, r) = current.handle(requestId, command)
      result = r
      next
    }
    result

  def plan: Plan = ledger.get.plan
  def usedSeats: Int = ledger.get.usedSeats
  def holders: Set[UserId] = ledger.get.holders
  def isLicensed(user: UserId): Boolean = ledger.get.holders.contains(user)
