Kjell
=====

Kjell - a refurbished Erlang shell with support for color profiles and extensions.						

## Screens

Kjell (dark profile) on Solarized Dark
![Kjell](http://karlll.github.io/kjell/images/kjell_demo_3_dark.png)

Kjell (light profile) on Solarized Light
![Kjell](http://karlll.github.io/kjell/images/kjell_demo_3_light.png)

## Overview

See http://karlll.github.com/kjell/

## Dependencies

* Erlang/OTP R16B01+
* Extension `kjell-prompt`: Powerline patched font (https://github.com/Lokaltog/powerline-fonts)

## Build
~~~
  $ git clone --recursive https://github.com/karlll/kjell.git && cd kjell
  $ make
~~~  

## Install

After building:
~~~
  $ make install
~~~  

### Install to alternate location
~~~
  $ make configure PREFIX=<alternate path>
  $ make install
~~~  

## Run

If installed, execute `kjell` in terminal, otherwise, `<build_dir>/bin/kjell`

## Extensions

Install extensions in user directory:
~~~   
  $ make install-extensions
~~~ 

See https://github.com/karlll/kjell-extensions for further information about extensions.

## Documentation

Please see the wiki, https://github.com/karlll/kjell/wiki

## Author

karlll, <karl@ninjacontrol.com>

## Contributors

* Pedram Nimreezi <deadzen@deadzen.com>
* Uwe Dauernheim <uwe@dauernheim.net>

## Build status

* Main branch (no yet)
* Development branch : [![Build Status](https://travis-ci.org/karlll/kjell.png?branch=develop)](https://travis-ci.org/karlll/kjell)


