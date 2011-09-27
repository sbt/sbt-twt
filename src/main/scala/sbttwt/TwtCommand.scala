package sbttwt

object TwtCommand {
  import sbt._
  import Keys._
  import complete.Parser
  import complete.DefaultParsers._
  import TwtProcessor._
  import dispatch.twitter.{Status, Twitter}
  
  def twt = Command("twt")(_ => twtCommand) { (state, cmd) =>
    cmd match {
      case Some(TwtLog(n)) =>
        auth_token map { t =>
          homeTimeline(n getOrElse {BigDecimal(defaultCount)}, None)(t)
        } getOrElse { get_authorization(None) }
      
      case Some(TwtUnread(n)) =>
        auth_token map { t =>
          homeTimeline(n getOrElse {BigDecimal(largeDefaultCount)}, loadSinceId)(t)
        } getOrElse { get_authorization(None) }
        
      case Some(TwtGrep(q, n)) => grep(q, n getOrElse {BigDecimal(defaultCount)})
      
      case Some(TwtCommit(s)) => 
        auth_token map { t =>
          tweet(s, None)(t)
        } getOrElse { get_authorization(None) }
      
      case Some(TwtReply(id, s)) =>
        auth_token map { t =>
          tweet(s, Some(id))(t)
        } getOrElse { get_authorization(None) }
        
      case Some(TwtRetweet(id)) =>
        auth_token map { t =>
          postRequest("retweeted", Status / "retweet/%s.json".format(id.toString))(t)
        } getOrElse { get_authorization(None) }

      case Some(TwtFave(id, unfave)) =>
        auth_token map { t =>
          if (!unfave) postRequest("faved", Twitter.host / "favorites/create/%s.json".format(id.toString))(t)
          else postRequest("unfaved", Twitter.host / "favorites/destroy/%s.json".format(id.toString))(t)
        } getOrElse { get_authorization(None) }
        
      case Some(TwtPin(n)) => get_authorization(Some(n))
      
      case Some(TwtClearAuth()) => 
        configFile.delete()
        println("OAuth credentials deleted.")
      
      case _ => usage
    }
    state
  }
  
  def usage {
    println("usage: twt <command>")
    println("  twt log [-12]             : prints 12 tweets from the timeline.")
    println("  twt unread [-100]         : prints unread tweets from the timeline.")
    println("  twt grep #scala [-12]     : searches for #scala. also as twt ?")
    println("  twt commit \"tweet!\"       : tweets quoted string. also as twt ci.")
    println("  twt re <id> \"tweet!\"      : tweets quoted string in reply to <id>.")
    println("  twt rt 118567297655902208 : retweets the tweet with <id>.")
    println("  twt fav <id> [-d]         : faves/unfaves the tweet with  <id>.")
    println("  twt pin 1234567           : authorizes twt to access twitter.")
    println("  twt clearauth             : clears the authorization.")
  }
    
  lazy val twtCommand: Parser[Option[TwtCommand]] =
    (Space ~> token(log | unread | pgrep | commit | reply | retweet | fave | pin | clearAuth))?
  lazy val log    = token("log" ~> countOption(defaultCount)) map { TwtLog(_)}
  lazy val unread = token("unread" ~> countOption(largeDefaultCount)) map { TwtUnread(_)}
  lazy val pgrep  = token((token("grep") | token("?")) ~> Space ~> token(NotSpace).examples("#scala") ~
    countOption(defaultCount)) map {
    case q ~ n =>  TwtGrep(q, n)
  }
  lazy val commit = (token("commit") | token("ci")) ~> Space ~> doubleQuoted.examples(""""tweet!"""") map { case s => TwtCommit(s) }
  lazy val reply  = (token("re") ~> Space ~> BigNat.examples("<id>") ~ Space ~ doubleQuoted.examples(""""tweet!"""")) map {
    case id ~ space ~ s => TwtReply(id, s)
  }
  lazy val retweet = token("rt") ~> Space ~> BigNat.examples("<id>") map { case id => TwtRetweet(id) }
  lazy val fave = token("fav") ~> Space ~> BigNat.examples("<id>") ~ dashD map {
    case id ~ unfave => TwtFave(id, unfave)    
  }
  lazy val pin    = token("pin" ~> Space ~> BigNat.examples("<pin>")) map { case x => TwtPin(x) }
  lazy val clearAuth = token("clearauth") ^^^ TwtClearAuth()
  
  def countOption(c: Int): Parser[Option[BigDecimal]] = token(Space ~> "-" ~> BigNat.examples(c.toString)).?
    
  lazy val BigNat: Parser[BigDecimal] = mapOrFail(Digit.+)( x => BigDecimal(x.mkString) )
  lazy val doubleQuoted: Parser[String] = token("\"") ~> charClass(_ != '\"', "non-double quote character").+.string <~("\"")  
  lazy val dashD: Parser[Boolean] = (Space ~> token("-d") ^^^ true).? map { _ getOrElse {false} }
}

sealed trait TwtCommand
case class TwtLog(n: Option[BigDecimal]) extends TwtCommand
case class TwtUnread(n: Option[BigDecimal]) extends TwtCommand
case class TwtGrep(q: String, n: Option[BigDecimal]) extends TwtCommand
case class TwtCommit(s: String) extends TwtCommand
case class TwtReply(id: BigDecimal, s: String) extends TwtCommand
case class TwtRetweet(id: BigDecimal) extends TwtCommand
case class TwtFave(id: BigDecimal, unfave: Boolean) extends TwtCommand
case class TwtPin(n: BigDecimal) extends TwtCommand
case class TwtClearAuth() extends TwtCommand
