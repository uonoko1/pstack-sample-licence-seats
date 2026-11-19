package licenceseats

enum SeatLimit:
  case Limited private[licence] (seats: Int)
  case Unlimited

enum Plan(val seatLimit: SeatLimit):
  case Free     extends Plan(SeatLimit.Limited(10))
  case Starter  extends Plan(SeatLimit.Limited(30))
  case Standard extends Plan(SeatLimit.Unlimited)
