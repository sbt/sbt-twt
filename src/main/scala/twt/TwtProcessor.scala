package twt

import sbt.{Project}
import sbt.processor._
import dispatch._
import json.JsHttp._
import oauth._
import twitter._

class TwitterProcessor extends Processor {
  // import and nickname Configgy's main access object
  import _root_.net.lag.configgy.{Configgy => C}
  // import all the methods, including implicit conversions, defined on dispatch.Http
  import Http._

  // this will be our datastore
  val conf = new java.io.File(System.getProperty("user.home"), ".twt.conf")
  // OAuth application key, top-secret
  val consumer = Consumer("Km1qVJOqYKDtFhlx6W6s3Q", "rgru7uaRGGm2fhrDrti4m8H5b8YliiohQyBcM17gBg")
  // one single-threaded http access point, please!
  val http = new Http

  val defaultCount = 12

  // ---BY YOUR COMMAND---
  def apply(label: String, project: Project, onFailure: Option[String], arg: String): ProcessorResult = {
    project.log.debug("TwitterProcessor(%s, %s, %s, %s)".format(label, project, onFailure, arg))
    def succeed(cmds: String*) = new Success(project, onFailure, cmds: _*)

    scan(arg.trim) match {
      case Left(msg) => project.log.error(msg)
      case Right(symbols) =>
        if (!conf.exists) buildDefaultConfig()

        C.configure(conf.getPath)

        val token = Token(C.config.configMap("access").asMap)
        project.log.debug(symbols.toString)
        symbols match {
          case Nil => usage

          case Unquoted("clearauth") :: Nil =>
            conf.delete()
            println("OAuth credentials deleted.")

          case Unquoted("log") :: Nil => token map { tok =>
              friendsTimeline(defaultCount, tok)
            } getOrElse { get_authorization(symbols) }
          case Unquoted("log") :: DashNumber(x) :: Nil => token map { tok =>
              friendsTimeline(x, tok)
            } getOrElse { get_authorization(symbols) }

          case Unquoted("commit") :: Quoted(tweet) :: Nil => token map { tok =>
              commit(tweet, tok)
            } getOrElse { get_authorization(symbols) }
          case Unquoted("ci") :: Quoted(tweet) :: Nil     => token map { tok =>
              commit(tweet, tok)
            } getOrElse { get_authorization(symbols) }

          case Unquoted("grep") :: Symbol(q) :: Nil                    => grep(q, defaultCount)
          case Unquoted("grep") :: Symbol(q) :: DashNumber(x) :: Nil   => grep(q, x)
          case Unquoted("search") :: Symbol(q) :: Nil                  => grep(q, defaultCount)
          case Unquoted("search") :: Symbol(q) :: DashNumber(x) :: Nil => grep(q, x)
          case Unquoted("?") :: Symbol(q)  :: Nil                      => grep(q, defaultCount)
          case Unquoted("?") :: Symbol(q)  :: DashNumber(x) :: Nil     => grep(q, x)

          case Unquoted("pin") :: Symbol(pin) :: Nil => get_authorization(symbols)
          case xs => usage
        }
    }

    succeed()
  }

  def usage {
    println("usage: twt <command>")
    println("  twt log [-12]         : prints 12 tweets from timeline except for RTs.")
    println("  twt grep #scala [-12] : searches for #scala. also as twt ?")
    println("  twt commit \"tweet!\"   : tweets quoted string. also as twt ci.")
    println("  twt pin 1234567       : authorizes twt to access twitter.")
    println("  twt clearauth         : clears the authorization.")
  }

  def friendsTimeline(count: Int, token: Token) {
    val messages = http(Status.friends_timeline(consumer, token, ("count", count)))
    for {
      item <- messages
      msg = Status.text(item)
      screen_name = Status.user.screen_name(item)
    } yield (formatTweet(msg, screen_name) map { println })
  }

  def formatTweet(tweet: String, screenName: String): List[String] =
    "* <" + screenName + ">" ::
    "- " + tweet ::
    "" :: Nil

  def grouped(tweet: String, size: Int): List[String] = {
    val buffer = new scala.collection.mutable.ListBuffer[String]
    var s = tweet
    while (s.length > size) {
      buffer += s take size
      s = s drop size
    }
    buffer += s
    buffer.toList
  }

  def grep(q: String, count: Int) {
    for {
      item <- http(Search(q, ("show_user", "true"), ("rpp", count)))
      msg = Search.text(item)
      from_user = Search.from_user(item)
    } yield (formatTweet(msg, from_user) map { println })
  }

  def commit(tweet: String, token: Token) {
    if (tweet.length > 140) println("%d characters?" format tweet.length)
    else http(Status.update(tweet, consumer, token) ># { js =>
      // handling the Status.update response as JSON, we take what we want
      val Status.user.screen_name(screen_name) = js
      val Status.id(id) = js

      // this goes back to our user
      println("posted " + (Twitter.host / screen_name / "status" / id.toString to_uri))
    })
  }

  def cat(token: Token) {
    // get us some tweets
    http(UserStream.open(consumer, token, None) { message =>
      import net.liftweb.json.JsonAST._
      // this listener is called each time a json message arrives

      // the friends message should be the first one to come in
      for (JArray(friends) <- message \ "friends") yield
        println("streaming tweets as they arrive...")

      // print apparent tweet if it has text and a screen_name
      for {
        JString(text) <- message \ "text"
        JString(name) <- message \ "user" \ "screen_name"
      } yield
        println("%-15s%s" format (name, text) )
    })
  }

  // oauth sesame
  def get_authorization(words: List[Symbol]) = {

    def validate(pin: String, token: Token) = try {
      // exchange it for an access token
      http(Auth.access_token(consumer, token, pin)) match {
        case (access_tok, _, screen_name) =>
          // nb: we're producing a message, a token type name, and the token itself
          ("approved! it's tweetin' time, %s." format screen_name, Some(("access", access_tok)))
      }
    } catch {
        case StatusCode(401, _) =>  ("rats! that pin %s doesn't seem to match." format pin, None)
    }

    // this time we are matching against a potential request token
    ((words, Token(C.config.configMap("request").asMap)) match {
      case (Unquoted("pin") :: Unquoted(pin) :: Nil, Some(token: Token)) => validate(pin, token)
      case (Unquoted("pin") :: Quoted(pin) :: Nil, Some(token: Token)) => validate(pin, token)

      // there wasn't a parameter so who cares if we have a request token, just get a new one
      case _ =>
        // a request token for the twt application, kthxbai
        val tok = http(Auth.request_token(consumer))
        // generate the url the user needs to go to, to grant us access
        val auth_uri = Auth.authorize_url(tok).to_uri
        (( try {
          // try to open it with the fancy desktop integration stuff,
          // using reflection so we can still compile on Java 5
          val dsk = Class.forName("java.awt.Desktop")
          dsk.getMethod("browse", classOf[java.net.URI]).invoke(
            dsk.getMethod("getDesktop").invoke(null), auth_uri
          )
          "accept the authorization request in your browser, for the fun to begin."
        } catch {
          // THAT went well. We'll just have to pass on that link to be printed.
          case _ =>
            "open the following URL in a browser to permit this application to tweet 4 u:\n%s".
              format(auth_uri.toString)
        }) + // so take one of those two messages and a general message
          "\n\nthen run `twt pin <pin>` to complete the process.\n",
          // and the request token that we got back from Twitter
          Some(("request", tok)))
    }) match // against the tuple produced by the last match
    {
      // no token, just a message to be printed up in `main`
      case (message, None) => println(message)

      // a token of some kind: we should save this in the datastore perhaps
      case (message, Some((name, tok))) =>
        val conf_writer = new java.io.FileWriter(conf)
        // let us also take this opportunity to set the log level
        conf_writer write (
        """ |<log>
            |  level = "ERROR"
            |  console = true
            |</log>
            |<%s>
            |  oauth_token = "%s"
            |  oauth_token_secret = "%s"
            |</%s>""".stripMargin format (name, tok.value, tok.secret, name)
        )
        conf_writer.close
        println(message) // for you sir!
    }
  }         // get_authorization

  def buildDefaultConfig() {
    conf.createNewFile()
    val conf_writer = new java.io.FileWriter(conf)
    conf_writer write (
    """ |<log>
        |  level = "ERROR"
        |  console = true
        |</log>""".stripMargin
    )
    conf_writer.close
  }

  trait Symbol { val value: String }
  object Symbol {
    def unapply(symbol: Symbol): Option[String] = symbol match {
      case DashValue(x) => None
      case _            => Some(symbol.value)
    }
  }

  object DashNumber {
    def unapply(symbol: Symbol): Option[Int] = symbol match {
      case DashValue(x) if x matches """\d+""" => Some(x.toInt)
      case _                                  => None
    }
  }

  case class Unquoted(value: String) extends Symbol
  case class Quoted(value: String) extends Symbol
  case class DashValue(value: String) extends Symbol

  abstract class ScanState
  case object NoWord extends ScanState
  case object InSQ extends ScanState
  case object InDQ extends ScanState
  case object InWord extends ScanState
  case object EndWord extends ScanState
  case class ScanError(value: String) extends ScanState
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

