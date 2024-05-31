type ParseResult[A] = Either[String, (String, A)]

class Parser[A](val parse: String => ParseResult[A]) {
  def repeated(min: Int, max: Int = Int.MaxValue): Parser[Seq[A]] = Parser(haystack =>
    var string = haystack

    val outputs = (1 to max).view.map { _ =>
      this.parse(string).map((leftover, output) =>
        string = leftover
        output
      )
    }.takeWhile(_.isRight).map(_.toOption.get).toSeq

    if outputs.size < min
    then Left(haystack)
    else Right((string, outputs))
  )

  def optional: Parser[Option[A]] = Parser(haystack =>
    this.parse(haystack) match
      case Right((rest, output)) => Right((rest, Some(output)))
      case Left(unparsed) => Right((unparsed, None)))

  def map[B](transform: A => B): Parser[B] = Parser(haystack =>
    this.parse(haystack).map((rest, output) => (rest, transform(output))))

  def filter(condition: A => Boolean): Parser[A] = Parser(haystack =>
    this.parse(haystack).filterOrElse((_, output) => condition(output), haystack))

  // should this return Either?
  // def or(that: Parser[A]): Parser[A] = Parser(haystack =>
  // this.parse(haystack).orElse(that.parse(haystack)))
  def or(that: Parser[A]): Parser[A] = Parser(haystack =>
    this.parse(haystack) match
      case Left(_) => that.parse(haystack)
      case other => other)

  def plus[B](that: Parser[B]): Parser[(A, B)] = Parser(haystack =>
    this.parse(haystack).flatMap((leftover, output1) =>
      that.map(output2 => (output1, output2)).parse(leftover)))

  def plusIgnore[B](that: Parser[B]): Parser[A] =
    this.plus(that).map((a, _) => a)

  def ignorePlus[B](that: Parser[B]): Parser[B] =
    this.plus(that).map((_, b) => b)

  def surroundedBy[B](that: Parser[B]): Parser[A] =
    that.ignorePlus(this).plusIgnore(that)

  //  def surroundedBy[B, C](that1: Parser[B], that2: Parser[C]): Parser[A] =
  //    that1.ignorePlus(this).plusIgnore(that2)
}

object Parser {
  def str(needle: String): Parser[String] = Parser(haystack =>
    if haystack.startsWith(needle)
    then Right((haystack.substring(needle.length), needle))
    else Left(haystack))

  def charIf(condition: Char => Boolean): Parser[Char] = Parser(haystack =>
    haystack.lift(0).filter(condition) match
      case Some(char) => Right(haystack.substring(1), char)
      case None => Left(haystack))

  def anyChar: Parser[Char] = Parser.charIf(_ => true)

  def whitespace: Parser[Char] = Parser.charIf(_.isWhitespace)

  def end: Parser[Unit] = Parser(haystack =>
    if haystack.isEmpty
    then Right("", ())
    else Left(haystack))

  // to avoid stack overflow, make it lazy
  def wrap[A](getParser: () => Parser[A]): Parser[A] =
    Parser(haystack => getParser().parse(haystack))
}