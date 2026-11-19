package licenceseats

final class Licences private (
    val plan: Plan,
    val holders: Set[UserId],
    handled: Map[RequestId, (Command, Result)]
):

  /** A request ID is handled once. Resending its request gets the first result and changes nothing. */
  def handle(request: Request): (Licences, Result) =
    handled.get(request.id) match
      case Some((request.command, firstResult)) => (this, firstResult)
      case Some(_) => (this, Left(Rejection.RequestIdReused))
      case None =>
        val decision = decide(request.command)
        val result = decision.map(_ => ())
        (decision.getOrElse(this).recorded(request, result), result)

  private def decide(command: Command): Either[Rejection, Licences] =
    command match
      case Command.Grant(user) =>
        exceeded(plan, holders + user) match
          case Some(seats) => Left(Rejection.SeatLimitReached(seats))
          case None => Right(Licences(plan, holders + user, handled))
      case Command.Revoke(user) =>
        Right(Licences(plan, holders - user, handled))
      case Command.ChangePlan(target) =>
        Right(Licences(target, holders, handled))

  /** The seat count that `newHolders` exceed. Only a limited plan has one. */
  private def exceeded(newPlan: Plan, newHolders: Set[UserId]): Option[Int] =
    newPlan.seatLimit match
      case SeatLimit.Limited(seats) if newHolders.size > seats => Some(seats)
      case SeatLimit.Limited(_) | SeatLimit.Unlimited => None

  private def recorded(request: Request, result: Result): Licences =
    Licences(plan, holders, handled.updated(request.id, (request.command, result)))

object Licences:
  def empty(plan: Plan): Licences = Licences(plan, Set.empty, Map.empty)
