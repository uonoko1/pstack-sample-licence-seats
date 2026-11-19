package licenceseats

/** A command plus the ID that makes resending it safe. */
final case class Request(id: RequestId, command: Command)

enum Command:
  /** The user ends up holding a licence. Granting to a current holder uses no extra seat. */
  case Grant(user: UserId)

  /** The user ends up holding no licence. Revoking from a non-holder changes nothing. */
  case Revoke(user: UserId)

  case ChangePlan(to: Plan)

enum Rejection:
  /** Granting would take the holder count past the plan's `seats`. */
  case SeatLimitReached(seats: Int)

  /** The target plan allows `seats`, fewer than the current `holders`. */
  case TooManyHolders(seats: Int, holders: Int)

  /** The request ID was first used for a different command, which was left as handled then. */
  case RequestIdReused
