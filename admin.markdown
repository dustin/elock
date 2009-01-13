---
layout: default
title: elock admin guide
---

# Getting Started

In order to use elock, you'll need the following:

* An [erlang/otp distribution][erlang] (**note**: this probably exists
  in your OS' package system).
* The [source][src] to elock itself.

## Building elock

With the [source][src] tarball downloaded and opened (or a fresh
checkout of the git repository), you should be able to just type
`make` within the source directory and have a build.

### Potential Error

You *may* get a build error if you're not using the exact version of
erlang the source tree is expecting.  Such an error will look like
this:

    stdlib: No valid version ("1.15.4") of .app file found.
    Found file "[erlang path]/stdlib.app" with version "1.15.2"

    kernel: No valid version ("2.12.4") of .app file found.
    Found file "[erlang path]/kernel.app" with version "2.12.2"

If you get this error, edit `lock_supervisor.rel` and replace the
versions of `kernel` and `stdlib` with the ones provided by your distribution.

## Running elock

After the software is built, you can try it out using the
`start_lock_serv` script.

By default, this starts an interactive erlang shell which can be
terminated by typing `q().`

In order to run it as a persistent process, you can add a couple of
parameters to that script:

    ./start_lock_serv -noinput -detached

or just build a custom start script that does the equivalent:

{% highlight sh %}
#!/bin/sh

cd /path/to/elock

/path/to/erl -noinput -detached -pa ebin -boot lock_serv
{% endhighlight %}

[erlang]:http://erlang.org/
[src]:http://github.com/dustin/elock/tarball/master "latest master"
