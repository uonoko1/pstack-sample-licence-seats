package licenceseats

import java.util.concurrent.atomic.AtomicReference

final class SpaceHolder(plan: Plan):
  private val state = AtomicReference(Space.empty(plan))

  def space: Space = state.get()

  def handle(request: Request): Result =
    val before = state.get()
    val (after, result) = before.handle(request)
    if state.compareAndSet(before, after) then result else handle(request)
