package licenceseats

opaque type UserId = String
object UserId:
  def apply(value: String): UserId = value
  extension (id: UserId) def value: String = id

opaque type RequestId = String
object RequestId:
  def apply(value: String): RequestId = value
  extension (id: RequestId) def value: String = id

/** How many seats a plan offers. */
enum SeatLimit:
  case Limited(seats: Int)
  case Unlimited

  /** Whether `usedSeats` licences fit within this limit. */
  def allows(usedSeats: Int): Boolean = this match
    case Limited(seats) => usedSeats <= seats
    case Unlimited      => true

enum Plan(val seatLimit: SeatLimit):
  case Free extends Plan(SeatLimit.Limited(10))
  case Starter extends Plan(SeatLimit.Limited(30))
  case Standard extends Plan(SeatLimit.Unlimited)

/** A state-changing request against a space's licences. */
enum Command:
  case Grant(user: UserId)
  case Revoke(user: UserId)
  case ChangePlan(plan: Plan)

/** Why a command was refused. A refused command leaves the state untouched. */
enum Rejection:
  /** Every seat of the current plan is taken. */
  case SeatLimitReached(plan: Plan, seats: Int)
  case AlreadyLicensed(user: UserId)
  case NotLicensed(user: UserId)
  /** The target plan has fewer seats than are currently in use. */
  case SeatsInUseExceedPlan(target: Plan, seats: Int, usedSeats: Int)
  /** The request ID was already used for a different command, so this is not a resend. */
  case RequestIdReused(requestId: RequestId, original: Command)

/** The plan and seat usage immediately after a command was applied. */
final case class Applied(plan: Plan, usedSeats: Int)

type Result = Either[Rejection, Applied]
