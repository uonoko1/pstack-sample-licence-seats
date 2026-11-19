package licenceseats

import java.time.Instant

enum Command:
  case Grant(user: UserId)
  case Revoke(user: UserId)
  case ChangePlan(plan: Plan)

/** `at` is the caller's current time. A resend is the same request whatever its `at`. */
final case class Request(id: RequestId, command: Command, at: Instant)

enum Rejection:
  case SeatLimitReached(seats: Int)

  /** The space is suspended, and its `holders` exceed the `seats` of the requested plan. */
  case Suspended(seats: Int, holders: Int)

  case RequestIdReused

type Result = Either[Rejection, Unit]
