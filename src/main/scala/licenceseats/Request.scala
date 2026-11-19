package licenceseats

import java.time.Instant

enum Command:
  case Grant(user: UserId)
  case Revoke(user: UserId)
  case ChangePlan(plan: Plan)

/** `at` is the caller's current time. The library reads no clock. */
final case class Request(id: RequestId, command: Command, at: Instant)

enum Rejection:
  case SeatLimitReached(seats: Int)
  /** The space is suspended and its `holders` still exceed the target plan's `seats`. */
  case Suspended(seats: Int, holders: Int)
  case RequestIdReused

type Result = Either[Rejection, Unit]
