sbt-twt
=======

sbt-twt is a twitter command for [simple-built-tool (sbt)][2], forked from @n8han's [dispatch twine][1].

## install it
add the following to `~/.sbt/plugins/build.sbt`:

    addSbtPlugin("com.eed3si9n" % "sbt-twt" % "0.2.0")

this adds a new command called `twt` with tab completion.

## uninstall it
remove the oauth token:

    sbt "twt clearauth"

remove the entry from global `build.sbt`.
    
## twt
all you have to do now, is start up `sbt`, and in the console type `twt <command>`.

    > twt
    usage: twt <command>
      twt log [-12]             : prints 12 tweets from the timeline.
      twt unread [-100]         : prints unread tweets from the timeline.
      twt grep #scala [-12]     : searches for #scala. also as twt ?
      twt commit "tweet!"       : tweets quoted string. also as twt ci.
      twt re <id> "tweet!"      : tweets quoted string in reply to <id>.
      twt rt 118567297655902208 : retweets the tweet with <id>.
      twt fav <id> [-d]         : faves/unfaves the tweet with  <id>.
      twt pin 1234567           : authorizes twt to access twitter.
      twt clearauth             : clears the authorization.
    > twt grep [tab]
    grep "#sbt #scala"   grep #scala
    > twt grep "#sbt #scala"
    * <bibixdev>             118465815925374978
    - bibixdev: #scala #sbt #github pure pleasure!

### oauth
since we don't want to either store passwords on machine or send them over wire, it uses OAuth.
and because this is not a web app, you'd need to type in a PIN number given to you by twitter.

    $ sbt
    ...
    > twt log
    accept the authorization request in your browser, for the fun to begin.
    (brower should pop up asking if you want to let sbt-twt access you account)
    then run `twt pin <pin>` to complete the process.
    > twt pin 8686743

## credits
most of the heavy lifting of talking to twitter was done by @n8han's [dispatch][4] library.

## the Unicode rendering issue (mojibakeh)
you need to add `-Dfile.encoding=UTF-8` in sbt's launch script to render non-ASCII correctly.

[1]: http://dispatch.databinder.net/Twine
[2]: https://github.com/harrah/xsbt
[4]: http://dispatch.databinder.net/
