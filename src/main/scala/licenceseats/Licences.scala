package licenceseats

import java.time.Duration
import java.time.Instant

enum Standing:
  case Normal
  case InGrace(deadline: Instant)
  case Suspended

final class Licences private (
    val plan: Plan,
    val holders: Set[UserId],
    deadline: Option[Instant],
    handled: Map[RequestId, (Command, Result)]
):

  def standing(at: Instant): Standing =
    deadline match
      case None => Standing.Normal
      case Some(due) if at.isBefore(due) => Standing.InGrace(due)
      case Some(_) => Standing.Suspended

  /** A request ID is handled once. Resending its request gets the first result and changes nothing, whatever its time. */
  def handle(request: Request): (Licences, Result) =
    handled.get(request.id) match
      case Some((request.command, firstResult)) => (this, firstResult)
      case Some(_) => (this, Left(Rejection.RequestIdReused))
      case None =>
        val decision = decide(request.command, request.at)
        val result = decision.map(_ => ())
        (decision.getOrElse(this).recorded(request, result), result)

  private def decide(command: Command, at: Instant): Either[Rejection, Licences] =
    command match
      case Command.Grant(user) =>
        exceeded(plan, holders + user).map(Rejection.SeatLimitReached(_)).toLeft(settled(plan, holders + user, at))
      case Command.Revoke(user) =>
        Right(settled(plan, holders - user, at))
      case Command.ChangePlan(target) =>
        Right(settled(target, holders, at))

  /** Every accepted change goes through here, so the deadline exists exactly while the holders exceed the seats, and the first one stays until they fit. */
  private def settled(newPlan: Plan, newHolders: Set[UserId], at: Instant): Licences =
    val newDeadline = exceeded(newPlan, newHolders).map(_ => deadline.getOrElse(at.plus(Licences.gracePeriod)))
    Licences(newPlan, newHolders, newDeadline, handled)

  /** The seat count that `newHolders` exceed on `newPlan`. Only a limited plan has one. */
  private def exceeded(newPlan: Plan, newHolders: Set[UserId]): Option[Int] =
    newPlan.seatLimit match
      case SeatLimit.Limited(seats) if newHolders.size > seats => Some(seats)
      case SeatLimit.Limited(_) | SeatLimit.Unlimited => None

  private def recorded(request: Request, result: Result): Licences =
    Licences(plan, holders, deadline, handled.updated(request.id, (request.command, result)))

object Licences:
  private val gracePeriod = Duration.ofDays(14)

  def empty(plan: Plan): Licences = Licences(plan, Set.empty, None, Map.empty)
