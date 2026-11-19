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

  def handleAll(requests: Seq[Request]): Seq[Result] =
    val (next, results) = requests.foldLeft((state.get(), Vector.empty[Result])) { case ((licences, results), request) =>
      val (after, result) = licences.handle(request)
      (after, results :+ result)
    }
    state.set(next)
    results
