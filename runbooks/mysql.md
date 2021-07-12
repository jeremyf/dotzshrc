If you need to have mysql@5.7 first in your PATH run:

`echo 'export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"' >> ~/.zshrc`

For compilers to find mysql@5.7 you may need to set:

`export LDFLAGS="-L/usr/local/opt/mysql@5.7/lib"`
`export CPPFLAGS="-I/usr/local/opt/mysql@5.7/include"`


For compilers to find mysql@5.7 you may need to set:
  export LDFLAGS="-L/usr/local/opt/mysql@5.7/lib"
  export CPPFLAGS="-I/usr/local/opt/mysql@5.7/include"

For pkg-config to find mysql@5.7 you may need to set:
  export PKG_CONFIG_PATH="/usr/local/opt/mysql@5.7/lib/pkgconfig"

For pkg-config to find mysql@5.7 you may need to set:

`export PKG_CONFIG_PATH="/usr/local/opt/mysql@5.7/lib/pkgconfig"`


To have launchd start mysql@5.7 now and restart at login:

`brew services start mysql@5.7`

Or, if you don't want/need a background service you can just run:

`/usr/local/opt/mysql@5.7/bin/mysql.server start`


## Installing with custom openssl

I needed to set some ldflags and cppflags when building the mysql2 gems:

`gem install mysql2 -v 0.4.8 -- --with-cflags=\"-I/usr/local/opt/openssl@1.1/include\" --with-ldflags=\"-L/usr/local/opt/openssl@1.1/lib\"`

`export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"`
`export CPPFLAGS="-L/usr/local/opt/openssl@1.1/include"`

## In the project's .bundler/config file:
`BUNDLE_BUILD__MYSQL2: "--with-mysql-config=/usr/local/opt/mysql@5.7/bin/mysql_config --with-ldflags=-L/usr/local/opt/openssl@1.1/lib --with-cppflags=-I/usr/local/opt/openssl@1.1/include --with-cflags=-I/usr/local/opt/openssl@1.1/include"`
