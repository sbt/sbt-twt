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

  // ---BY YOUR COMMAND---
  def apply(label: String, project: Project, onFailure: Option[String], arg: String): ProcessorResult = {
    project.log.debug("TwitterProcessor(%s, %s, %s, %s)".format(label, project, onFailure, arg))
    def succeed(cmds: String*) = new Success(project, onFailure, cmds: _*)

    scan(arg.trim) match {
      case Left(msg) => project.log.error(msg)
      case Right(symbols) =>
        conf.createNewFile()
        C.configure(conf.getPath)

        val token = Token(C.config.configMap("access").asMap)

        symbols match {
          case Nil => usage

          case Unquoted("clearautho") :: Nil =>
            conf.delete()
            println("OAuth credentials deleted.")

          case Unquoted("log") :: Nil => token map {
              friendsTimeline
            } getOrElse { get_authorization(symbols) }

          case Unquoted("commit") :: Quoted(tweet) :: Nil => token map { tok =>
              commit(tweet, tok)
            } getOrElse { get_authorization(symbols) }

          case Unquoted("pin") :: Unquoted(pin) :: Nil => get_authorization(symbols)
          case Unquoted("pin") :: Quoted(pin) :: Nil => get_authorization(symbols)

          case xs => usage
        }
    }

    succeed()
  }

  def usage {
    println("usage: twt <command>")
    println("  twt log              : prints your timeline except for RTs.")
    println("  twt commit \"tweet!\": tweets quoted string.")
    println("  twt pin 123456       : authorizes twt to access twitter.")
    println("  twt clearauth        : clears the authorization.")
  }

  def friendsTimeline(token: Token) {
    val messages = http(Status.friends_timeline(consumer, token, ("count", 20)))
    for {
      item <- messages
      msg = Status.text(item)
      screen_name = Status.user.screen_name(item)
    } yield println("%-15s%s" format (screen_name.toString, msg) )
  }

  def commit(tweet: String, token: Token): String =
    if (tweet.length > 140) "%d characters?" format tweet.length
    else http(Status.update(tweet, consumer, token) ># { js =>
      // handling the Status.update response as JSON, we take what we want
      val Status.user.screen_name(screen_name) = js
      val Status.id(id) = js

      // this goes back to our user
      "posted " + (Twitter.host / screen_name / "status" / id.toString to_uri)
    })

  def cat(token: Token) {
    // get us some tweets
    http(UserStream.open(consumer, token, None) { message =>
      import net.liftweb.json.JsonAST._
      // this listener is called each time a json message arrives

      // the friends message should be the first one to come in
      for (JArray(friends) <- message \ "friends") yield
        println("Streaming tweets as they arrive...")

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
          ("Approved! It's tweetin' time, %s." format screen_name, Some(("access", access_tok)))
      }
    } catch {
        case StatusCode(401, _) =>  ("Rats! That PIN %s doesn't seem to match." format pin, None)
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
          "Accept the authorization request in your browser, for the fun to begin."
        } catch {
          // THAT went well. We'll just have to pass on that link to be printed.
          case _ =>
            "Open the following URL in a browser to permit this application to tweet 4 u:\n%s".
              format(auth_uri.toString)
        }) + // so take one of those two messages and a general message
          "\n\nThen run `twt <pin>` to complete authorization.\n",
          // and the request token that we got back from Twitter
          Some(("request", tok)))
    }) match // against the tuple produced by the last match
    {
      // no token, just a message to be printed up in `main`
      case (message, None) => message
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
        message // for you sir!
    }
  }         // get_authorization

  trait Symbol { val value: String }
  case class Unquoted(value: String) extends Symbol
  case class Quoted(value: String) extends Symbol
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

    def grab(st: ScanState) {
      words append Quoted(word.toString)
      word = new StringBuilder
      state = st
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
        case (x, InSQ) if x == '\''         => grab(EndWord)
        case (x, InSQ)                      => unescape(x)
        case (x, InDQ) if x == '\"'         => grab(EndWord)
        case (x, InDQ)                      => unescape(x)
        case (x, InWord) if x == '\''       => state = ScanError("Unexpected \'")
        case (x, InWord) if x == '\"'       => state = ScanError("Unexpected \"")
        case (x, InWord) if x.isWhitespace  => grab(NoWord)
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
        if (!word.toString.isEmpty)
          words append Unquoted(word.toString)

        Right(words.toList)
    }
  }
}

