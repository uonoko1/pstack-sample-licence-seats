package licenceseats

import java.time.Duration
import java.time.Instant

final class Licences private (
    val plan: Plan,
    val holders: Set[UserId],
    graceDeadline: Option[Instant],
    handled: Map[RequestId, (Command, Result)]
):

  def standingAt(time: Instant): Standing =
    graceDeadline match
      case None => Standing.Normal
      case Some(deadline) if time.isBefore(deadline) => Standing.InGrace(deadline)
      case Some(_) => Standing.Suspended

  /** A request ID is handled once. Resending its request gets the first result and changes nothing. */
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
        exceeded(plan, holders + user) match
          case Some(seats) => Left(Rejection.SeatLimitReached(seats))
          case None => Right(updated(plan, holders + user, at))
      case Command.Revoke(user) =>
        Right(updated(plan, holders - user, at))
      case Command.ChangePlan(target) =>
        (standingAt(at), exceeded(target, holders)) match
          case (Standing.Suspended, Some(seats)) => Left(Rejection.Suspended(seats, holders.size))
          case _ => Right(updated(target, holders, at))

  /** The seat count that `newHolders` exceed. Only a limited plan has one. */
  private def exceeded(newPlan: Plan, newHolders: Set[UserId]): Option[Int] =
    newPlan.seatLimit match
      case SeatLimit.Limited(seats) if newHolders.size > seats => Some(seats)
      case SeatLimit.Limited(_) | SeatLimit.Unlimited => None

  /** The only place that sets the grace deadline. It runs exactly while the holders exceed the plan. */
  private def updated(newPlan: Plan, newHolders: Set[UserId], at: Instant): Licences =
    val deadline = exceeded(newPlan, newHolders).map(_ => graceDeadline.getOrElse(at.plus(Licences.gracePeriod)))
    Licences(newPlan, newHolders, deadline, handled)

  private def recorded(request: Request, result: Result): Licences =
    Licences(plan, holders, graceDeadline, handled.updated(request.id, (request.command, result)))

object Licences:
  private val gracePeriod = Duration.ofDays(14)

  def empty(plan: Plan): Licences = Licences(plan, Set.empty, None, Map.empty)
