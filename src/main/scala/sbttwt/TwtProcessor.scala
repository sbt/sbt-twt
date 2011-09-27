package sbttwt

object TwtProcessor {
  import dispatch._
  import json._
  import json.JsHttp._
  import oauth._
  import oauth.OAuth._
  import twitter._

  // import all the methods, including implicit conversions, defined on dispatch.Http
  import Http._

  val home = :/("twitter.com")
  // this will be our datastore
  val configFile = new java.io.File(System.getProperty("user.home"), ".twt.xml")
  // OAuth application key, top-secret
  val consumer = Consumer("Km1qVJOqYKDtFhlx6W6s3Q", "rgru7uaRGGm2fhrDrti4m8H5b8YliiohQyBcM17gBg")
  // one single-threaded http access point, please!
  val http = new Http with NoLogging

  val defaultCount = 12
  val largeDefaultCount = 100
  
  val config = new Config(configFile)
  def auth_token: Option[Token] = Token(config.section("access").toMap)
  
  def homeTimeline(count: BigDecimal, sinceId: Option[BigDecimal])(implicit token: Token) {
    val param = sinceId map { si =>
      Map("count" -> count.toString, "since_id" -> si.toString)
    } getOrElse { Map("count" -> count.toString) }
    val statuses = http(Status / "home_timeline.json" <<? param
        <@ (consumer, token) ># (list ! obj) )
    statuses match {
      case x :: xs =>
        val Status.id(id) = x
        saveSinceId(id)
      case _ => // do nothing
    }

    for {
      item <-statuses
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
  
  def grep(q: String, count: BigDecimal) {
    for {
      item <- http(Search(q, ("show_user", "true"), ("rpp", count.toString)))
      id = Search.id(item)
      msg = Search.text(item)
      from_user = Search.from_user(item)
    } yield (formatTweet(id, msg, from_user) map { println })
  }  
  
  def tweet(status: String, inReplyTo: Option[BigDecimal])(implicit token: Token) {
    val param = inReplyTo map { re =>
      Map("in_reply_to_status_id" -> re.toString, "status" -> status)
    } getOrElse { Map("status" -> status) }

    if (status.length > 140) println("%d characters?" format status.length)
    else http(Status / "update.json" << param <@ (consumer, token) ># { js =>
      println("posted " + statusUri(js)) })
  }
  
  def statusUri(js: JsValue) = {
    val Status.user.screen_name(screen_name) = js
    val Status.id(id) = js
    home / screen_name / "status" / id.toString to_uri
  }
  
  def postRequest(verb: String, req: Request)(implicit token: Token) =
    http(req << Map.empty[String, String] <@ (consumer, token) ># { js =>
      println(verb + " " + statusUri(js)) })  
  
  def loadSinceId: Option[BigDecimal] =
    config.section("statuses").get("since_id") map { s => BigDecimal(s) }
  
  def saveSinceId(sinceId: BigDecimal) {
    config.section("statuses")("since_id") = sinceId.toString
    config.write()
  }
    
  def formatTweet(id: BigDecimal, status: String, screenName: String): List[String] =
    "* %-22s %s".format("<" + screenName + ">", id.toString) ::
    "- " + Status.rebracket(status) ::
    "" :: Nil      
  
  /*
  def friendsTimeline(count: BigDecimal)(implicit token: Token) {
    val messages = http(Status.friends_timeline(consumer, token, ("count", count)))
    for {
      item <- messages
      id = Status.id(item)
      msg = Status.text(item)
      screen_name = Status.user.screen_name(item)
    } yield (formatTweet(id, msg, screen_name) map { println })
  }

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
  */

  // oauth sesame
  def get_authorization(pin: Option[BigDecimal]) = {

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
    ((pin, Token(config.section("request").toMap)) match {
      case (Some(pin), Some(token: Token)) => validate(pin.toString, token)

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
        config.section(name)("oauth_token") =  tok.value
        config.section(name)("oauth_token_secret") = tok.secret
        config.write()

        println(message) // for you sir!
    }
  }         // get_authorization
}
