package twt

trait Symbol { val value: String }
object Symbol {
  def unapply(symbol: Symbol): Option[String] = symbol match {
    case DashValue(x) => None
    case _            => Some(symbol.value)
  }

  def scan(input: String): Either[String, List[Symbol]] = {
    var word = new StringBuilder
    val words = new scala.collection.mutable.ListBuffer[Symbol]
    val it = input.elements
    var state: ScanState = NoWord

    def grabQuoted {
      words append Quoted(word.toString)
      word = new StringBuilder
      state = EndWord
    }

    def grabUnquoted {
      if (word.toString startsWith "-") words append DashValue(word.toString drop 1)
      else words append Unquoted(word.toString)
      word = new StringBuilder
      state = NoWord
    }

    def unescape(c: Char) {
      c match {
        case '\\' if !it.hasNext => state = ScanError("Unexpected \\")
        case '\\'                => it.next match {
          case '\\' => word += '\\'
          case 'n'  => word += '\n'
          case 't'  => word += '\t'
          case 'r'  => word += '\r'
          case '\'' => word += '\''
          case '\"' => word += '\"'
          case x    => word += x
        }
        case x                   => word += x
      }
    }

    while (it.hasNext) {
      (it.next, state) match {
        case (x, NoWord) if x == '\''       => state = InSQ
        case (x, NoWord) if x == '\"'       => state = InDQ
        case (x, NoWord) if x.isWhitespace  => // nothing
        case (x, NoWord)                    => word += x
                                               state = InWord
        case (x, InSQ) if x == '\''         => grabQuoted
        case (x, InSQ)                      => unescape(x)
        case (x, InDQ) if x == '\"'         => grabQuoted
        case (x, InDQ)                      => unescape(x)
        case (x, InWord) if x == '\''       => state = ScanError("Unexpected \'")
        case (x, InWord) if x == '\"'       => state = ScanError("Unexpected \"")
        case (x, InWord) if x.isWhitespace  => grabUnquoted
        case (x, InWord)                    => word += x
        case (x, EndWord) if x.isWhitespace => state = NoWord
        case (x, EndWord)                   => state = ScanError("Expected space but met " + x)
        case _ =>
      }
    }

    state match {
      case InSQ         => Left("Expected \' but met EOL")
      case InDQ         => Left("Expected \" but met EOL")
      case e: ScanError => Left(e.value)
      case _ =>
        if (!word.toString.isEmpty) grabUnquoted

        Right(words.toList)
    }
  }
}

object DashNumber {
  def unapply(symbol: Symbol): Option[BigDecimal] = symbol match {
    case DashValue(x) if x matches """\d+""" => Some(BigDecimal(x))
    case _                                   => None
  }
}

case class Unquoted(value: String) extends Symbol

object UnquotedNumber {
  def unapply(symbol: Symbol): Option[BigDecimal] = symbol match {
    case Unquoted(x) if x matches """\d+""" => Some(BigDecimal(x))
    case _                                  => None
  }
}

case class Quoted(value: String) extends Symbol
case class DashValue(value: String) extends Symbol

abstract class ScanState
case object NoWord extends ScanState
case object InSQ extends ScanState
case object InDQ extends ScanState
case object InWord extends ScanState
case object EndWord extends ScanState
case class ScanError(value: String) extends ScanState
