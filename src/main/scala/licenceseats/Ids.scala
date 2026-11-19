package licenceseats

opaque type UserId = String

object UserId:
  def apply(value: String): UserId = value

  extension (id: UserId) def value: String = id

opaque type RequestId = String

object RequestId:
  def apply(value: String): RequestId = value

  extension (id: RequestId) def value: String = id
