package caller

class IllegalStatesSuite extends munit.FunSuite:

  test("a request takes a request ID and a user ID, each in its own place") {
    assertEquals(compileErrors("""import licenceseats.*; Request(RequestId("r"), java.time.Instant.EPOCH, Command.Grant(UserId("u")))"""), "")
  }

  test("a user ID does not compile where a request ID is expected") {
    val errors = compileErrors("""import licenceseats.*; Request(UserId("r"), java.time.Instant.EPOCH, Command.Grant(UserId("u")))""")
    assert(clue(errors).contains("Required: licenceseats.RequestId"))
  }

  test("a request ID does not compile where a user ID is expected") {
    val errors = compileErrors("""import licenceseats.*; Command.Grant(RequestId("u"))""")
    assert(clue(errors).contains("Required: licenceseats.UserId"))
  }

  test("a plain string does not compile where an ID is expected") {
    val errors = compileErrors("""import licenceseats.*; Command.Grant("u")""")
    assert(clue(errors).contains("Required: licenceseats.UserId"))
  }

  test("a state cannot be built with holders that bypass the seat limit") {
    val errors = compileErrors("""import licenceseats.*; new Licences(Plan.Free, Set(UserId("u")), None, Map.empty)""")
    assert(clue(errors).contains("cannot be accessed"))
  }

  test("a state cannot be built with a grace deadline that disagrees with its plan and holders") {
    val errors = compileErrors(
      """import licenceseats.*; new Licences(Plan.Free, Set.empty, Some(java.time.Instant.EPOCH), Map.empty)"""
    )
    assert(clue(errors).contains("cannot be accessed"))
  }
