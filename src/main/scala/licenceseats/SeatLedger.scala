package licenceseats

import java.time.{Duration, Instant}

/** Immutable licence state of one space, together with the results of the requests it has handled.
  *
  * All rules live here; [[SpaceLicences]] only adds thread-safe mutation on top. The ledger never
  * reads a clock: every decision is made against the `now` the caller passes in.
  *
  * @param graceDeadline
  *   defined exactly while more licences are held than the plan offers; it is the instant the
  *   grace period that started with the overage runs out. [[handle]] keeps this in step with the
  *   plan and the holders.
  */
final case class SeatLedger private (
    plan: Plan,
    holders: Set[UserId],
    graceDeadline: Option[Instant],
    handled: Map[RequestId, (Command, Result)]
):
  def usedSeats: Int = holders.size

  /** The standing of the space at `now`, judged from the current plan and holders. */
  def standing(now: Instant): Standing = graceDeadline match
    case None                                     => Standing.Normal
    case Some(deadline) if now.isBefore(deadline) => Standing.InGrace(deadline)
    case Some(deadline)                           => Standing.Suspended(deadline)

  /** Applies `command` at `now`, or replays the stored result when `requestId` has been handled
    * before.
    *
    * Rejections are stored as well: a resend keeps returning the original rejection even if the
    * command would succeed by now. `now` is not part of the request's identity, so a resend made
    * at a later time is still a resend.
    */
  def handle(requestId: RequestId, command: Command, now: Instant): (SeatLedger, Result) =
    handled.get(requestId) match
      case Some((`command`, result)) => (this, result)
      case Some((original, _))       => (this, Left(Rejection.RequestIdReused(requestId, original)))
      case None =>
        val (next, result) = decide(command, now) match
          case Left(rejection) => (this, Left(rejection))
          case Right(changed) =>
            val settled = changed.withGraceSettled(now)
            (settled, Right(Applied(settled.plan, settled.usedSeats, settled.standing(now))))
        (next.copy(handled = handled.updated(requestId, (command, result))), result)

  private def decide(command: Command, now: Instant): Either[Rejection, SeatLedger] = command match
    case Command.Grant(user) =>
      if holders.contains(user) then Left(Rejection.AlreadyLicensed(user))
      else
        plan.seatLimit match
          // Also covers a space that is over its limit, in grace or suspended.
          case SeatLimit.Limited(seats) if usedSeats >= seats =>
            Left(Rejection.SeatLimitReached(plan, seats))
          case _ => Right(copy(holders = holders + user))

    case Command.Revoke(user) =>
      if holders.contains(user) then Right(copy(holders = holders - user))
      else Left(Rejection.NotLicensed(user))

    case Command.ChangePlan(target) =>
      (target.seatLimit, standing(now)) match
        case (SeatLimit.Limited(seats), Standing.Suspended(_)) if usedSeats > seats =>
          Left(Rejection.SeatsInUseExceedPlan(target, seats, usedSeats))
        case _ => Right(copy(plan = target))

  /** Starts a grace period when the space has just gone over its limit, keeps the running one
    * while it stays over, and clears it once the holders fit again.
    */
  private def withGraceSettled(now: Instant): SeatLedger =
    if plan.seatLimit.allows(usedSeats) then copy(graceDeadline = None)
    else if graceDeadline.isDefined then this
    else copy(graceDeadline = Some(now.plus(SeatLedger.GracePeriod)))

object SeatLedger:
  /** How long a space may hold more licences than its plan offers before it is suspended. */
  val GracePeriod: Duration = Duration.ofDays(14)

  def empty(plan: Plan): SeatLedger = SeatLedger(plan, Set.empty, None, Map.empty)
