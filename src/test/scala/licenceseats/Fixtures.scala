package licenceseats

import java.time.Duration
import java.time.Instant
import java.util.UUID

trait Fixtures:

  private val start = Instant.parse("2026-01-01T00:00:00Z")

  def day(n: Int): Instant = start.plus(Duration.ofDays(n))

  def user(n: Int): UserId = UserId(s"user-$n")

  def users(count: Int): Set[UserId] = (1 to count).map(user).toSet

  def fresh(command: Command, at: Instant = day(0)): Request =
    Request(RequestId(UUID.randomUUID().toString), command, at)

  def withHolders(plan: Plan, count: Int): Licences =
    users(count).foldLeft(Licences.empty(plan)) { (licences, holder) =>
      licences.handle(fresh(Command.Grant(holder)))._1
    }
