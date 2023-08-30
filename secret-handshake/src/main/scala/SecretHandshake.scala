object SecretHandshake {

  def commands(code: Int): List[String] = {
    val bitOps: List[(Int, String)] = List(
      0x01 -> "wink",
      0x02 -> "double blink",
      0x04 -> "close your eyes",
      0x08 -> "jump"
    )

    val ops = for {
      (bit, op) <- bitOps
      if (code & bit) == bit
    } yield op

    if ((code & 0x10) == 0x10) ops.reverse else ops
  }
}
