package licenceseats

import java.time.Instant

/** Where a space stands at one moment. It is read from the licences and a time, and never stored. */
enum Standing:
  case Normal
  case InGrace(deadline: Instant)
  case Suspended
