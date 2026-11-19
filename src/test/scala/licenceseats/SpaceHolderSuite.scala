package licenceseats

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

class SpaceHolderSuite extends munit.FunSuite:

  private val freeSeats = 10
  private val threads   = 16

  private def concurrently[A](tasks: Seq[() => A]): Seq[A] =
    val pool  = Executors.newFixedThreadPool(threads)
    val start = CountDownLatch(1)
    try
      given ExecutionContext = ExecutionContext.fromExecutor(pool)
      val running = tasks.map { task =>
        Future {
          start.await()
          task()
        }
      }
      start.countDown()
      Await.result(Future.sequence(running), 30.seconds)
    finally
      pool.shutdownNow()
      pool.awaitTermination(5, TimeUnit.SECONDS)
      ()

  test("the holder applies requests to its space") {
    val holder = SpaceHolder(Plan.Free)
    assertEquals(holder.handle(Request(RequestId("a"), Command.Grant(UserId("u")))), Right(()))
    assertEquals(holder.space.plan, Plan.Free)
    assertEquals(holder.space.holders, Set(UserId("u")))
  }

  test("concurrent grants never exceed the seat limit and none is lost") {
    val contenders = 200
    (1 to 50).foreach { round =>
      val holder = SpaceHolder(Plan.Free)
      val users  = (1 to contenders).map(n => UserId(s"user-$n"))
      val results = concurrently(users.map { user => () =>
        user -> holder.handle(Request(RequestId(s"grant-$user"), Command.Grant(user)))
      })

      val accepted = results.collect { case (user, Right(())) => user }
      val rejected = results.collect { case (_, Left(rejection)) => rejection }
      assertEquals(accepted.size, freeSeats, s"round $round")
      assertEquals(holder.space.holders, accepted.toSet, s"round $round")
      assertEquals(
        rejected,
        Seq.fill(contenders - freeSeats)(Rejection.SeatLimitReached(freeSeats)),
        s"round $round"
      )
    }
  }

  test("concurrent resends of one request ID all get the same result and one effect") {
    val holder = SpaceHolder(Plan.Free)
    (1 until freeSeats).foreach { n =>
      holder.handle(Request(RequestId(s"seed-$n"), Command.Grant(UserId(s"seed-$n"))))
    }
    val results = concurrently((1 to 200).map { n => () =>
      holder.handle(Request(RequestId("shared"), Command.Grant(UserId(s"user-$n"))))
    })
    assertEquals(results.distinct, Seq(Right(()): Result))
    assertEquals(holder.space.holders.size, freeSeats)
  }
