package licenceseats

import java.time.Duration
import java.time.Instant

/** `graceDeadline` is set exactly while the holders exceed the plan's seats. It dates from the moment they first did. */
final class Licences private (
    val plan: Plan,
    val holders: Set[UserId],
    graceDeadline: Option[Instant],
    handled: Map[RequestId, (Command, Result)]
):

  /** The grace period lasts up to its deadline. From the deadline on, the space is suspended. */
  def standing(at: Instant): Standing =
    graceDeadline match
      case None => Standing.Normal
      case Some(deadline) if at.isBefore(deadline) => Standing.InGrace(deadline)
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
        val granted = holders + user
        plan.seatLimit.exceededBy(granted.size) match
          case Some(seats) => Left(Rejection.SeatLimitReached(seats))
          case None => Right(moved(plan, granted, at))
      case Command.Revoke(user) =>
        Right(moved(plan, holders - user, at))
      case Command.ChangePlan(target) =>
        target.seatLimit.exceededBy(holders.size) match
          case Some(seats) if standing(at) == Standing.Suspended =>
            Left(Rejection.SpaceSuspended(seats, holders.size))
          case _ => Right(moved(target, holders, at))

  /** Every accepted command passes through here, so the deadline always agrees with the plan and holders. */
  private def moved(newPlan: Plan, newHolders: Set[UserId], at: Instant): Licences =
    val deadline = newPlan.seatLimit
      .exceededBy(newHolders.size)
      .map(_ => graceDeadline.getOrElse(at.plus(Licences.gracePeriod)))
    Licences(newPlan, newHolders, deadline, handled)

  private def recorded(request: Request, result: Result): Licences =
    Licences(plan, holders, graceDeadline, handled.updated(request.id, (request.command, result)))

object Licences:
  private val gracePeriod = Duration.ofDays(14)

  def empty(plan: Plan): Licences = Licences(plan, Set.empty, None, Map.empty)
