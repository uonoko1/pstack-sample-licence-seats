package licenceseats

import java.util.concurrent.{Callable, CyclicBarrier, ExecutorService, Executors}
import scala.jdk.CollectionConverters.*

class LicenceManagerSuite extends munit.FunSuite:
  private val contenders = 32
  private val pool = FunFixture[ExecutorService](
    setup = _ => Executors.newFixedThreadPool(contenders),
    teardown = _.shutdownNow()
  )

  /** Submits every request at once, one thread each, and returns each request's result. */
  private def submitTogether(
      exec: ExecutorService,
      manager: LicenceManager,
      requests: Seq[Request]
  ): Seq[(Request, Either[Rejection, Unit])] =
    val start = CyclicBarrier(requests.size)
    val tasks = requests.map { request =>
      new Callable[(Request, Either[Rejection, Unit])]:
        def call() =
          start.await()
          request -> manager.submit(request)
    }
    exec.invokeAll(tasks.asJava).asScala.toSeq.map(_.get)

  pool.test("concurrent grants on Free accept exactly 10 and hold exactly those users"): exec =>
    (1 to 200).foreach { round =>
      val manager = LicenceManager(Plan.Free)
      val requests = (1 to contenders).map(n => Request(RequestId(s"r$round-$n"), Command.Grant(UserId(s"user-$n"))))
      val results = submitTogether(exec, manager, requests)

      val granted = results.collect { case (Request(_, Command.Grant(u)), Right(())) => u }
      val rejections = results.collect { case (_, Left(rejection)) => rejection }
      assertEquals(granted.size, 10, s"round $round")
      assertEquals(rejections, Seq.fill(contenders - 10)(Rejection.SeatLimitReached(10)), s"round $round")
      assertEquals(manager.current.holders, granted.toSet, s"round $round")
    }
