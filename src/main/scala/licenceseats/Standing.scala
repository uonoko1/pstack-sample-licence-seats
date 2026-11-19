package licenceseats

import java.time.Instant

/** In grace until `deadline` while the holders exceed the seat limit. Suspended from `deadline` on. */
enum Standing:
  case Normal
  case InGrace(deadline: Instant)
  case Suspended
