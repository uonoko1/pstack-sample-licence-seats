package licenceseats

enum SeatLimit:
  case Limited(seats: Int)
  case Unlimited

  /** The seat count that `holders` exceed. Only a limited plan has one. */
  def exceededBy(holders: Int): Option[Int] =
    this match
      case Limited(seats) if holders > seats => Some(seats)
      case Limited(_) | Unlimited => None

enum Plan(val seatLimit: SeatLimit):
  case Free extends Plan(SeatLimit.Limited(10))
  case Starter extends Plan(SeatLimit.Limited(30))
  case Standard extends Plan(SeatLimit.Unlimited)
