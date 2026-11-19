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

  /** A downgrade may leave more holders than seats. No grant is accepted until they fit again. */
  private def decide(command: Command): Either[Rejection, Licences] =
    command match
      case Command.Grant(user) =>
        val granted = holders + user
        plan.seatLimit.exceededBy(granted.size) match
          case Some(seats) => Left(Rejection.SeatLimitReached(seats))
          case None => Right(Licences(plan, granted, handled))
      case Command.Revoke(user) =>
        Right(Licences(plan, holders - user, handled))
      case Command.ChangePlan(target) =>
        Right(Licences(target, holders, handled))

  private def recorded(request: Request, result: Result): Licences =
    Licences(plan, holders, handled.updated(request.id, (request.command, result)))

object Licences:
  def empty(plan: Plan): Licences = Licences(plan, Set.empty, Map.empty)
