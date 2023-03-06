package Frontend

import parsley.errors.tokenextractors
import parsley.errors.ErrorBuilder

object Errors {

  case class Error(
    pos: (Int, Int), 
    source: String, 
    errorLines: ErrorLines
  ) {
    override def toString: String = {
      s"""${errorLines.errorType}:
        |in file $source at line ${pos._1}, column ${pos._2}
        |${errorLines.lines.mkString("\n")}
        |${errorLines.lineInfo.toSeq.mkString("\n")}
      """.stripMargin
    }
  }

  sealed trait ErrorLines {
    val errorType: String
    val lines: Seq[String]
    val lineInfo: LineInfo
  }

  case class SyntaxError(
    unexpected: Option[String],
    expected: Option[String],
    msgs: Seq[String],
    lineInfo: LineInfo
  ) extends ErrorLines {
    override val errorType = "Syntax Error"
    override val lines: Seq[String] = (unexpected, expected) match {
      case (None, None) => msgs.toList
      case _ =>
        "unexpected: " + unexpected.getOrElse("") :: "expected: " + expected
          .getOrElse("") :: msgs.toList
    }
  }

  case class SemanticError(
    unexpected: Option[String],
    expected: Option[String],
    msgs: Seq[String],
    lineInfo: LineInfo
  ) extends ErrorLines {
    override val errorType = "Semantic Error"
    override val lines: Seq[String] = (unexpected, expected) match {
      case (None, None) => msgs.toList
      case _ =>
        "unexpected: " + unexpected.getOrElse("") :: "expected: " + expected
          .getOrElse("") :: msgs.toList
    }
  }

    object SemanticError {
    def genError(
      msg: String,
      pos: (Int, Int)
    )(implicit
      source: String,
      fileLines: Array[String]
    ): Error = {
      Error(pos, source, new SemanticError(None, None, Seq(msg), LineInfo.getFrom(pos)))
    }
  }

  case class LineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int
  ) {
    def toSeq: Seq[String] = {
      linesBefore.map(line => s">$line") ++:
        Seq(s">$line", s"${" " * errorPointsAt}^") ++:
        linesAfter.map(line => s">$line")
    }
  }

  object LineInfo{
    def getFrom(pos: (Int, Int))(implicit fileLines: Array[String]): LineInfo = pos match {
      case (line, col) if (line >= 1) => LineInfo(
        fileLines(line - 1),
        if (line > 1) Seq(fileLines(line - 2)) else Nil,
        if (line < fileLines.length) Seq(fileLines(line)) else Nil,
        col
      )
    }
  }

  class ErrorB extends ErrorBuilder[Error] with tokenextractors.MatchParserDemand {

    override def format(
      pos: Position, 
      source: Source, 
      lines: ErrorInfoLines
    ): Error = Error(pos, source, lines)

    type Position = (Int, Int)
    override type Source = String
    override def pos(line: Int, col: Int): Position = (line, col)
    override def source(sourceName: Option[String]): Source = sourceName.getOrElse("")

    type ErrorInfoLines = ErrorLines

    override def vanillaError(
      unexpected: UnexpectedLine, 
      expected: ExpectedLine, 
      msgs: Messages, 
      line: LineInfo
    ): ErrorInfoLines = SyntaxError(
      unexpected, 
      expected, 
      msgs,
      line
    )

    override def specialisedError(
      msgs: Messages, 
      line: LineInfo
      ): ErrorInfoLines = SyntaxError(
        None, 
        None, 
        msgs, 
        line
      )

    type ExpectedItems = Option[String]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = 
      if (alts.isEmpty) None else Some(alts.mkString(", "))

    type Messages = Seq[Message]
    override def combineMessages(alts: Seq[Message]): Messages = alts

    type UnexpectedLine = Option[String]
    override def unexpected(item: Option[Item]): UnexpectedLine = item
    
    type ExpectedLine = Option[String]
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    type LineInfo = Errors.LineInfo
    override def lineInfo(
      line: String, 
      linesBefore: Seq[String], 
      linesAfter: Seq[String], 
      errorPointsAt: Int,
      errorWidth: Int
    ): LineInfo = LineInfo(
      line, 
      linesBefore, 
      linesAfter, 
      errorPointsAt
    )

    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String
    override def raw(item: String): Raw = 
      item match {
        case xs if xs.head.isWhitespace =>
          xs.head match {
            case x if x.isSpaceChar => "space"
            case '\n'               => "newline"
            case '\t'               => "tab"
            case _                  => "whitespace character"
          }
        case xs => "\"" + xs.takeWhile(!_.isWhitespace) + "\""
      }
    
    override def named(item: String): Named = item
    override val endOfInput: EndOfInput = "end of input"

    override val numLinesBefore: Int = 1
    override val numLinesAfter: Int = 1
  }
}