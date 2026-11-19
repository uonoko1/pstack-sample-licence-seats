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
        fitted(plan, holders + user).left.map(Rejection.SeatLimitReached(_))
      case Command.Revoke(user) =>
        Right(Licences(plan, holders - user, handled))
      case Command.ChangePlan(target) =>
        fitted(target, holders).left.map(Rejection.TooManyHolders(_, holders.size))

  /** Left is the seat count that `newHolders` exceed. Only a limited plan has one. */
  private def fitted(newPlan: Plan, newHolders: Set[UserId]): Either[Int, Licences] =
    newPlan.seatLimit match
      case SeatLimit.Limited(seats) if newHolders.size > seats => Left(seats)
      case SeatLimit.Limited(_) | SeatLimit.Unlimited => Right(Licences(newPlan, newHolders, handled))

  private def recorded(request: Request, result: Result): Licences =
    Licences(plan, holders, handled.updated(request.id, (request.command, result)))

object Licences:
  def empty(plan: Plan): Licences = Licences(plan, Set.empty, Map.empty)
