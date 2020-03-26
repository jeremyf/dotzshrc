When installing `libxml-ruby`, I encountered the following error in my `mkmf.log`

```console
    #include <libxml/xmlversion.h>
             ^~~~~~~~~~~~~~~~~~~~~
    1 error generated.
```

To remediate I took the following steps:

## Step 1

```console
gem install libxml2
```

## Step 2

In shell profile. Note the exports are commented out but available as
a possible reference

```console
if [ -d /usr/local/opt/libxml2/bin ]; then
  export PATH="/usr/local/opt/libxml2/bin:$PATH"
  # export LDFLAGS="-L/usr/local/opt/libxml2/lib"
  # export CPPFLAGS="-I/usr/local/opt/libxml2/include"
fi
```

## Step 3

For the given bundler project, add the following:

```console
bundle config build.libxml-ruby --use-system-libraries \
  --with-xml2-include=$(brew --prefix libxml2)/include/libxml2
```
