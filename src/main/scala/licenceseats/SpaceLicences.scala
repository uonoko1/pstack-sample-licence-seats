package licenceseats

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

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
    transact(_.handle(requestId, command, now))

  /** Applies `commands` in order as one atomic step: no other request is interleaved with them,
    * and each result is the one a later resend of that request ID will get.
    */
  def submitAll(commands: Seq[(RequestId, Command)], now: Instant): Seq[Result] =
    transact { start =>
      commands.foldLeft((start, Vector.empty[Result])) { case ((current, results), (requestId, command)) =>
        val (after, result) = current.handle(requestId, command, now)
        (after, results :+ result)
      }
    }

  /** The only way the ledger is ever written. `step` computes the next state and an outcome from
    * one snapshot, and the pair is published only if nobody else changed the state in between;
    * otherwise it is recomputed from the newer state. So an outcome is never returned for a state
    * that was not stored, and no concurrent update is overwritten.
    */
  @tailrec
  private def transact[A](step: SeatLedger => (SeatLedger, A)): A =
    val current = ledger.get
    val (next, outcome) = step(current)
    if ledger.compareAndSet(current, next) then outcome else transact(step)

  def plan: Plan = ledger.get.plan

  /** The standing of the space at `now`: normal, in grace with its deadline, or suspended. */
  def standing(now: Instant): Standing = ledger.get.standing(now)

  def usedSeats: Int = ledger.get.usedSeats
  def holders: Set[UserId] = ledger.get.holders
  def isLicensed(user: UserId): Boolean = ledger.get.holders.contains(user)
