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

    // create config file if it doesn't exist
    conf.createNewFile()
    // read config file to C.config
    C.configure(conf.getPath)

    val words = (label.trim split "\\s+").toList.tail

    println(words.toString)

    // This is it, people. All paths return to println with a message for the user,
    // except `cat` which doesn't. We're going to pattern-match against both the
    // parameter sequence and a Token that is either there or not there. The
    // dispatch.oauth.Token(m: Map[...]) method knows about maps with token keys
    // in them. If these are present under "access", we'll get Some(token)
    println( (words, Token(C.config.configMap("access").asMap)) match {
      // the only parameter was "reset"; ignore the token and delete the data store
      case (List("reset"), _) => conf.delete(); "OAuth credentials deleted."

      // there are no parameters, but we have a token! Go into `cat`, forever.
      case (Nil, Some(tok: Token)) => cat(tok)

      // there are some parameters and a token, combine parameters and...
      case (xs, Some(tok: Token)) => (xs mkString " ") match {
        // dang tweet is too long
        case tweet if tweet.length > 140 =>
          "%d characters? This is Twitter not NY Times Magazine." format tweet.length

//        // it looks like an okay tweet, let us post it:
//        case tweet => http(Status.update(tweet, consumer, tok) ># { js =>
//          // handling the Status.update response as JSON, we take what we want
//          val Status.user.screen_name(screen_name) = js
//          val Status.id(id) = js
//          // this goes back to our user
//          "Posted: " + (Twitter.host / screen_name / "status" / id.toString to_uri)
//        })
      }
      // there was no access token, we must still be in the oauthorization process
      case (xs, _) => get_authorization(xs)
    })

    succeed()
  }

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
  def get_authorization(words: List[String]) = {
    // this time we are matching against a potential request token
    ((words, Token(C.config.configMap("request").asMap)) match {
      // one parameter that must be the verifier, and there's a request token
      case (List(verifier), Some(tok: Token)) => try {
        // exchange it for an access token
        http(Auth.access_token(consumer, tok, verifier)) match {
          case (access_tok, _, screen_name) =>
            // nb: we're producing a message, a token type name, and the token itself
            ("Approved! It's tweetin' time, %s." format screen_name, Some(("access", access_tok)))
        } } catch {
          // accidents happen
          case StatusCode(401, _) =>
            // no token for you
            ("Rats! That PIN %s doesn't seem to match." format verifier, None)
        }
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
            |  level = "WARNING"
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
}

