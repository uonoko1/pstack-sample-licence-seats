package licenceseats

enum Command:
  case Grant(user: UserId)
  case Revoke(user: UserId)

final case class Request(id: RequestId, command: Command)

enum Rejection:
  case SeatLimitReached(seats: Int)

type Result = Either[Rejection, Unit]
