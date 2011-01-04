package twt

import sbt.{Project}
import sbt.processor._
import dispatch._
import json._
import json.JsHttp._
import oauth._
import oauth.OAuth._
import twitter._

class TwitterProcessor extends Processor {
  // import and nickname Configgy's main access object
  import _root_.net.lag.configgy.{Configgy => C}
  // import all the methods, including implicit conversions, defined on dispatch.Http
  import Http._

  val home = :/("twitter.com")
  // this will be our datastore
  val conf = new java.io.File(System.getProperty("user.home"), ".twt.conf")
  // OAuth application key, top-secret
  val consumer = Consumer("Km1qVJOqYKDtFhlx6W6s3Q", "rgru7uaRGGm2fhrDrti4m8H5b8YliiohQyBcM17gBg")
  // one single-threaded http access point, please!
  val http = new Http

  val defaultCount = 12

  // sbt interface
  def apply(label: String, project: Project, onFailure: Option[String], args: String): ProcessorResult = {
    project.log.debug("TwitterProcessor(%s, %s, %s, %s)".format(label, project, onFailure, args))
    def succeed(cmds: String*) = new Success(project, onFailure, cmds: _*)
    Symbol.scan(args.trim) match {
      case Left(msg)      => project.log.error(msg)
      case Right(symbols) =>
        project.log.debug(symbols.toString)
        apply(symbols)
    }
    succeed()
  }

  def apply(symbols: List[Symbol]) {
    if (!conf.exists) buildDefaultConfig()
    C.configure(conf.getPath)
    val token = Token(C.config.configMap("access").asMap)
    val commitCmd = List("commit", "ci")
    val grepCmd = List("grep", "search", "?")

    symbols match {
      case Nil => usage

      case Unquoted("log") :: options => token map { t => implicit val tok = t
          homeTimeline(count(options))
        } getOrElse { get_authorization(symbols) }

      case Unquoted(s) :: Quoted(tweet) :: Nil if commitCmd contains s => token map { t => implicit val tok = t
          commit(tweet)
        } getOrElse { get_authorization(symbols) }

      case Unquoted("rt") :: UnquotedNumber(id) :: Nil => token map { t => implicit val tok = t
          postRequest("retweeted", Status / "retweet/%s.json".format(id.toString))
        } getOrElse { get_authorization(symbols) }

      case Unquoted("fav") :: UnquotedNumber(id) :: Nil => token map { t => implicit val tok = t
          postRequest("faved", Twitter.host / "favorites/create/%s.json".format(id.toString))
        } getOrElse { get_authorization(symbols) }

      case Unquoted("fav") :: UnquotedNumber(id) :: DashValue("d") :: Nil => token map { t =>
           implicit val tok = t
           postRequest("unfaved", Twitter.host / "favorites/destroy/%s.json".format(id.toString))
         } getOrElse { get_authorization(symbols) }

      case Unquoted(s) :: Symbol(q) :: options if grepCmd contains s => grep(q, count(options))

      case Unquoted("clearauth") :: Nil =>
        conf.delete()
        println("OAuth credentials deleted.")
      case Unquoted("pin") :: UnquotedNumber(pin) :: Nil => get_authorization(symbols)
      case xs => usage
    }
  }

  def count(options: List[Symbol]): BigDecimal = (options flatMap { // 2.7 doesn't have collect?
    case DashNumber(x) => Some(x)
    case _             => None
  }).toList match {
    case x :: xs => x
    case _       => defaultCount
  }

  def usage {
    println("usage: twt <command>")
    println("  twt log [-12]            : prints 12 tweets from the timeline.")
    println("  twt grep #scala [-12]    : searches for #scala. also as twt ?")
    println("  twt commit \"tweet!\"      : tweets quoted string. also as twt ci.")
    println("  twt rt 21499972767715328 : retweets the tweet with given id.")
    println("  twt fav <id> [-d]        : faves/unfaves the tweet with given id.")
    println("  twt pin 1234567          : authorizes twt to access twitter.")
    println("  twt clearauth            : clears the authorization.")
  }

  def homeTimeline(count: BigDecimal)(implicit token: Token) {
    for {
      item <- http(Status / "home_timeline.json" <<? Map("count" -> count)
        <@ (consumer, token) ># (list ! obj) )
      id = Status.id(item)
      msg = Status.text(item)
      screen_name = Status.user.screen_name(item)
      retweeted_status = ('retweeted_status ?? obj) (item)
    } yield
      (for {
        item <- retweeted_status
        id = Status.id(item)
        msg = Status.text(item)
        screen_name = Status.user.screen_name(item)
      } yield formatTweet(id, msg, screen_name) ) getOrElse {
        formatTweet(id, msg, screen_name) } map { println }
  }

  def friendsTimeline(count: BigDecimal)(implicit token: Token) {
    val messages = http(Status.friends_timeline(consumer, token, ("count", count)))
    for {
      item <- messages
      id = Status.id(item)
      msg = Status.text(item)
      screen_name = Status.user.screen_name(item)
    } yield (formatTweet(id, msg, screen_name) map { println })
  }

  def formatTweet(id: BigDecimal, tweet: String, screenName: String): List[String] =
    "* %-22s %s".format("<" + screenName + ">", id.toString) ::
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

  def grep(q: String, count: BigDecimal) {
    for {
      item <- http(Search(q, ("show_user", "true"), ("rpp", count)))
      id = Search.id(item)
      msg = Search.text(item)
      from_user = Search.from_user(item)
    } yield (formatTweet(id, msg, from_user) map { println })
  }

  def commit(tweet: String)(implicit token: Token) {
    if (tweet.length > 140) println("%d characters?" format tweet.length)
    else http(Status.update(tweet, consumer, token) ># { js =>
      println("posted " + statusUri(js)) })
  }

  def statusUri(js: JsValue) = {
    val Status.user.screen_name(screen_name) = js
    val Status.id(id) = js
    home / screen_name / "status" / id.toString to_uri
  }

  def postRequest(verb: String, req: Request)(implicit token: Token) =
    http(req << Map.empty[String, Any] <@ (consumer, token) ># { js =>
      println(verb + " " + statusUri(js)) })

  def cat(implicit token: Token) {
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
      case (Unquoted("pin") :: Symbol(pin) :: Nil, Some(token: Token)) => validate(pin, token)

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
}
