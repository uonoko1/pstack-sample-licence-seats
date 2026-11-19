package licenceseats

class IllegalStatesSuite extends munit.FunSuite:

  test("a request takes a request ID and a user ID, each in its own place") {
    assertEquals(compileErrors("""Request(RequestId("r"), Command.Grant(UserId("u")))"""), "")
  }

  test("a user ID does not compile where a request ID is expected") {
    val errors = compileErrors("""Request(UserId("r"), Command.Grant(UserId("u")))""")
    assert(clue(errors).contains("Required: licenceseats.RequestId"))
  }

  test("a request ID does not compile where a user ID is expected") {
    val errors = compileErrors("""Command.Grant(RequestId("u"))""")
    assert(clue(errors).contains("Required: licenceseats.UserId"))
  }

  test("a plain string does not compile where an ID is expected") {
    val errors = compileErrors("""Command.Grant("u")""")
    assert(clue(errors).contains("Required: licenceseats.UserId"))
  }

  test("a state cannot be built with holders that bypass the seat limit") {
    val errors = compileErrors("""new Licences(Plan.Free, Set(UserId("u")), Map.empty)""")
    assert(clue(errors).contains("cannot be accessed"))
  }
