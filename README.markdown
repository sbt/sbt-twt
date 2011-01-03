sbt-twt
=======

sbt-twt is a twitter processor for [simple-built-tool (sbt)][2], forked from @n8han's [dispatch twine][1].
unlike sbt plugins, [processors][3] are installed to the sbt instance instead of described in project definitions,
and they add new kind of commands, instead of new type of projects or actions that plugins give.

## install it
`sbt` uses some special commands that are prefixed with '*'.

    git clone git://github.com/eed3si9n/sbt-twt.git
    cd sbt-twt
    sbt publish-local
    sbt "*twt is com.eed3si9n sbt-twt 0.1"

the above adds a new command called `twt`.

## uninstall it

    sbt "twt clearauth"
    sbt "*remove twt"

## upgrade it
run `sbt "*remove twt"`, then install it.

## twt
all you have to do now, is start up `sbt`, and in the console type `twt <command>`.

    > twt
    usage: twt <command>
      twt log [-12]            : prints 12 tweets from the timeline.
      twt grep #scala [-12]    : searches for #scala. also as twt ?
      twt commit "tweet!"      : tweets quoted string. also as twt ci.
      twt rt 21499972767715328 : retweets the tweet with given id.
      twt pin 1234567          : authorizes twt to access twitter.
      twt clearauth            : clears the authorization.
    > twt grep sbt-twt -1
    * <ymnk>
    - RT @eed3si9n: tweeting from sbt using https://github.com/eed3si9n/sbt-twt #scala

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
all of the heavy lifting of talking to twitter was done by @n8han's [dispatch][4] library.

## the Unicode rendering issue (mojibakeh)
you need to add `-Dfile.encoding=UTF-8` in sbt's launch script to render non-ASCII correctly.

[1]: http://dispatch.databinder.net/Twine
[2]: http://code.google.com/p/simple-build-tool/
[3]: http://code.google.com/p/simple-build-tool/wiki/Processors
[4]: http://dispatch.databinder.net/About
