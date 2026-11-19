package licenceseats

enum Command:
  case Grant(user: UserId)
  case Revoke(user: UserId)
  case ChangePlan(plan: Plan)

final case class Request(id: RequestId, command: Command)

enum Rejection:
  case SeatLimitReached(seats: Int)
  case RequestIdReused

type Result = Either[Rejection, Unit]
