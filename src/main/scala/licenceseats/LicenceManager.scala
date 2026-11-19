package licenceseats

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

final class LicenceManager(plan: Plan):
  private val state = AtomicReference(Licences.empty(plan))

  def licences: Licences = state.get()

  @tailrec
  def handle(request: Request): Result =
    val current = state.get()
    val (next, result) = current.handle(request)
    if state.compareAndSet(current, next) then result else handle(request)
