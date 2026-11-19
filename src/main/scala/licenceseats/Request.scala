package licenceseats

opaque type UserId = String
object UserId:
  def apply(value: String): UserId = value

opaque type RequestId = String
object RequestId:
  def apply(value: String): RequestId = value

enum Command:
  case Grant(user: UserId)
  case Revoke(user: UserId)
  case ChangePlan(plan: Plan)

final case class Request(id: RequestId, command: Command)

enum Rejection:
  case SeatLimitReached(seats: Int)
  case TooManyHolders(holders: Int, seats: Int)

type Result = Either[Rejection, Unit]
