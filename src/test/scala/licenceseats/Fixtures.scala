package licenceseats

import java.util.UUID

trait Fixtures:

  def user(n: Int): UserId = UserId(s"user-$n")

  def users(count: Int): Set[UserId] = (1 to count).map(user).toSet

  def fresh(command: Command): Request =
    Request(RequestId(UUID.randomUUID().toString), command)

  def withHolders(plan: Plan, count: Int): Licences =
    users(count).foldLeft(Licences.empty(plan)) { (licences, holder) =>
      licences.handle(fresh(Command.Grant(holder)))._1
    }
