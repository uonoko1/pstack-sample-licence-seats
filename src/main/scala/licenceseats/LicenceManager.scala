package licenceseats

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

final class LicenceManager(plan: Plan):
  private val state = AtomicReference(Licences.empty(plan))

  def licences: Licences = state.get()

  def handle(request: Request): Result = atomically(_.handle(request))

  def handleAll(requests: Seq[Request]): Seq[Result] =
    atomically { start =>
      requests.foldLeft((start, Vector.empty[Result])) { case ((licences, results), request) =>
        val (after, result) = licences.handle(request)
        (after, results :+ result)
      }
    }

  @tailrec
  private def atomically[A](change: Licences => (Licences, A)): A =
    val current = state.get()
    val (next, result) = change(current)
    if state.compareAndSet(current, next) then result else atomically(change)
