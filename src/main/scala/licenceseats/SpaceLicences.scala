package licenceseats

/** One space's licences. Every value obeys `plan`'s seat limit, because only `empty` and `handle` build one. */
final case class SpaceLicences private (
    plan: Plan,
    holders: Set[UserId],
    private val handled: Map[RequestId, SpaceLicences.Handled]
):
  import SpaceLicences.Handled

  /** Applies `request`, or replays the result recorded for its ID. */
  def handle(request: Request): (SpaceLicences, Either[Rejection, Unit]) =
    handled.get(request.id) match
      case Some(Handled(command, result)) if command == request.command => (this, result)
      case Some(_) => (this, Left(Rejection.RequestIdReused))
      case None =>
        val (next, result) = execute(request.command)
        (next.copy(handled = handled.updated(request.id, Handled(request.command, result))), result)

  private def execute(command: Command): (SpaceLicences, Either[Rejection, Unit]) =
    command match
      case Command.Grant(user) =>
        val granted = holders + user
        plan.seatLimit match
          case SeatLimit.Limited(seats) if granted.size > seats =>
            (this, Left(Rejection.SeatLimitReached(seats)))
          case _ => (copy(holders = granted), Right(()))
      case Command.Revoke(user) => (copy(holders = holders - user), Right(()))
      case Command.ChangePlan(to) =>
        to.seatLimit match
          case SeatLimit.Limited(seats) if holders.size > seats =>
            (this, Left(Rejection.TooManyHolders(seats, holders.size)))
          case _ => (copy(plan = to), Right(()))

object SpaceLicences:
  def empty(plan: Plan): SpaceLicences = new SpaceLicences(plan, Set.empty, Map.empty)

  private final case class Handled(command: Command, result: Either[Rejection, Unit])
