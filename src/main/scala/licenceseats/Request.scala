package licenceseats

import java.time.Instant

enum Command:
  case Grant(user: UserId)
  case Revoke(user: UserId)
  case ChangePlan(plan: Plan)

/** `at` is the caller's current time. The library reads no clock of its own. */
final case class Request(id: RequestId, at: Instant, command: Command)

enum Rejection:
  case SeatLimitReached(seats: Int)
  case TooManyHolders(seats: Int, holders: Int)
  case RequestIdReused

type Result = Either[Rejection, Unit]
