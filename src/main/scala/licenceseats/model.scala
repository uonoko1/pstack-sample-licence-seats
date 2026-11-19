package licenceseats

import java.time.Instant

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
  /** The space is suspended and the target plan still has fewer seats than are in use. */
  case SeatsInUseExceedPlan(target: Plan, seats: Int, usedSeats: Int)
  /** The request ID was already used for a different command, so this is not a resend. */
  case RequestIdReused(requestId: RequestId, original: Command)

/** Where a space stands with respect to its seat limit at some instant. */
enum Standing:
  /** The licences held fit within the plan. */
  case Normal

  /** More licences are held than the plan offers; the overage must be resolved before `deadline`.
    * Grants are refused meanwhile.
    */
  case InGrace(deadline: Instant)

  /** The grace period ran out at `since` with the overage unresolved. Grants and changes to a plan
    * the holders still do not fit are refused until the holders fit again.
    */
  case Suspended(since: Instant)

/** The plan, seat usage and standing immediately after a command was applied. */
final case class Applied(plan: Plan, usedSeats: Int, standing: Standing)

type Result = Either[Rejection, Applied]
