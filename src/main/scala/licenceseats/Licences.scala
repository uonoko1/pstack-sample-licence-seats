package licenceseats

final class Licences private (val plan: Plan, val holders: Set[UserId]):

  def handle(request: Request): (Licences, Result) =
    val decision = decide(request.command)
    (decision.getOrElse(this), decision.map(_ => ()))

  private def decide(command: Command): Either[Rejection, Licences] =
    command match
      case Command.Grant(user) =>
        fitted(plan, holders + user).left.map(Rejection.SeatLimitReached(_))
      case Command.Revoke(user) =>
        Right(Licences(plan, holders - user))

  /** Left is the seat count that `newHolders` exceed. Only a limited plan has one. */
  private def fitted(newPlan: Plan, newHolders: Set[UserId]): Either[Int, Licences] =
    newPlan.seatLimit match
      case SeatLimit.Limited(seats) if newHolders.size > seats => Left(seats)
      case SeatLimit.Limited(_) | SeatLimit.Unlimited => Right(Licences(newPlan, newHolders))

object Licences:
  def empty(plan: Plan): Licences = Licences(plan, Set.empty)
