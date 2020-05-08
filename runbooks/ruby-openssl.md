## Determine if Ruby and Shell have same OpenSSL

```shell
$ openssl version
$ ruby -ropenssl -e 'puts OpenSSL::OPENSSL_VERSION'
```

## Tell RBEnv Which OpenSSL to Use

```shell
$ export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
$ export RUBY_CONFIGURE_OPTS=--with-openssl-dir=/usr/local/Cellar/openssl@1.1/1.1.1g/
$ rbenv install 2.7.1
```

I don't know if PATH should always be set to that.

## More Notes

I'm not quite certain what's going on. But I'm trying to document some things.

My understanding is that Mac OSX's OpenSSL implementation (LibreSSL 2.8.3) does not provide all of the functionality needed in Ruby and other Open Source projects.

So rbenv requires that you download openssl-1.1.1. Homebrew's most recent version of openssl appears to be 1.1.1g.

However, when I went to install a ruby version via `rbenv install 2.6.6`, it immediately outputs the following:

```
Downloading openssl-1.1.1d.tar.gz...
```

Hmm. Maybe this was the problem. So I made sure to tell Ruby where exactly to find openssl

```shell
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export RUBY_CONFIGURE_OPTS=--with-openssl-dir=/usr/local/Cellar/openssl@1.1/1.1.1g/
```

And I ran `rbenv install 2.6.6`. I did not get the `downloading openssl-1.1.1d.tar.gz`

When rbenv finished, I had compatable version in shell and console

```shell
$ openssl version
OpenSSL 1.1.1g  21 Apr 2020
```

```shell
$ ruby -ropenssl -e 'puts OpenSSL::OPENSSL_VERSION'
OpenSSL 1.1.1g  21 Apr 2020
```

The problems that I encountered?

I was attempting to post, via Ruby, a Webmention to two different webmention end points. This was working a week ago, but started silently failing.

Both services responded with a 400 HTTP status. For one, I got a `Webmention must mention a valid page` from one service and `source or target were missing` error from another.

When I ran: `curl -i -d "source=SOURCE_URL&target=TARGET_URL" WEBMENTION_URL` it worked.
