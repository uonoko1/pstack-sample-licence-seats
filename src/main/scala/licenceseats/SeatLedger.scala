package licenceseats

/** Immutable licence state of one space, together with the results of the requests it has handled.
  *
  * All rules live here; [[SpaceLicences]] only adds thread-safe mutation on top.
  */
final case class SeatLedger private (
    plan: Plan,
    holders: Set[UserId],
    handled: Map[RequestId, (Command, Result)]
):
  def usedSeats: Int = holders.size

  /** Applies `command`, or replays the stored result when `requestId` has been handled before.
    *
    * Rejections are stored as well: a resend keeps returning the original rejection even if the
    * command would succeed by now.
    */
  def handle(requestId: RequestId, command: Command): (SeatLedger, Result) =
    handled.get(requestId) match
      case Some((`command`, result)) => (this, result)
      case Some((original, _))       => (this, Left(Rejection.RequestIdReused(requestId, original)))
      case None =>
        val (next, result) = decide(command) match
          case Left(rejection) => (this, Left(rejection))
          case Right(changed)  => (changed, Right(Applied(changed.plan, changed.usedSeats)))
        (next.copy(handled = handled.updated(requestId, (command, result))), result)

  private def decide(command: Command): Either[Rejection, SeatLedger] = command match
    case Command.Grant(user) =>
      if holders.contains(user) then Left(Rejection.AlreadyLicensed(user))
      else
        plan.seatLimit match
          case SeatLimit.Limited(seats) if usedSeats >= seats =>
            Left(Rejection.SeatLimitReached(plan, seats))
          case _ => Right(copy(holders = holders + user))

    case Command.Revoke(user) =>
      if holders.contains(user) then Right(copy(holders = holders - user))
      else Left(Rejection.NotLicensed(user))

    case Command.ChangePlan(target) =>
      target.seatLimit match
        case SeatLimit.Limited(seats) if usedSeats > seats =>
          Left(Rejection.SeatsInUseExceedPlan(target, seats, usedSeats))
        case _ => Right(copy(plan = target))

object SeatLedger:
  def empty(plan: Plan): SeatLedger = SeatLedger(plan, Set.empty, Map.empty)
