package licenceseats

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/** Thread-safe home for one space's licences. Each request applies atomically through `SpaceLicences.handle`. */
final class LicenceManager(plan: Plan):
  private val state = AtomicReference(SpaceLicences.empty(plan))

  def current: SpaceLicences = state.get

  @tailrec
  def submit(request: Request): Either[Rejection, Unit] =
    val before = state.get
    val (after, result) = before.handle(request)
    if state.compareAndSet(before, after) then result else submit(request)
