package licenceseats

import java.time.Instant
import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

class LicenceManagerSuite extends munit.FunSuite:

  private val contenders = 64
  private val rounds = 200
  private val now = Instant.parse("2026-01-01T00:00:00Z")

  private val pool = FunFixture[ExecutorService](
    setup = _ => Executors.newFixedThreadPool(contenders),
    teardown = _.shutdownNow()
  )

  /** Holds every task at one gate, then releases them together. */
  private def race[A](pool: ExecutorService, tasks: Seq[() => A]): Seq[A] =
    val ready = CountDownLatch(tasks.size)
    val go = CountDownLatch(1)
    val pending = tasks.map { task =>
      val gated: Callable[A] = () =>
        ready.countDown()
        go.await()
        task()
      pool.submit(gated)
    }
    ready.await()
    go.countDown()
    pending.map(_.get(10, TimeUnit.SECONDS))

  pool.test("concurrent grants to different users never exceed the seat limit") { pool =>
    for _ <- 1 to rounds do
      val manager = LicenceManager(Plan.Free)

      val results = race(
        pool,
        (1 to contenders).map { n =>
          val user = UserId(s"user-$n")
          () => user -> manager.handle(Request(RequestId(s"grant-$n"), Command.Grant(user), now))
        }
      )

      val accepted = results.collect { case (user, Right(())) => user }
      val rejections = results.collect { case (_, Left(rejection)) => rejection }
      assertEquals(accepted.size, 10)
      assertEquals(rejections, Seq.fill(54)(Rejection.SeatLimitReached(10)))
      assertEquals(manager.licences.holders, accepted.toSet)
  }
