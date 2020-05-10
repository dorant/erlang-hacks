# Erlang hacks

## Projects
* frogger - A frogger game in Erlang
* redis-usage - Test of eredis
* hex - binary to/from hex converter functions
* [echo server](echo_server/README.md) - Collection of different echo server designs

## Setup

### Install erlang

``` bash
# See https://github.com/kerl/kerl
brew install kerl
# Build with wxWidgets by first installing
sudo apt-cache search 'libwxgt*'
sudo apt-get -y install libwxgtk3.0-gtk3-dev
kerl build 22.1
kerl install 22.1 ~/erlang/22.1
. ~/erlang/22.1/activate

# Install rebar3
curl -L https://github.com/erlang/rebar3/releases/download/3.13.0/rebar3 -o ~/bin/rebar3 && chmod +x ~/bin/rebar3

# Install elvis
cd /tmp; git clone https://github.com/inaka/elvis.git; cd elvis; rebar3 escriptize; cp _build/default/bin/elvis ~/erlang/22.2/bin/erl
```

### Run

``` shell
# Run
rebar3 shell
```

### Create new project

``` bash
rebar3 new app <name>
rebar3 new lib <name>

# Style reviewer
elvis rock
```

### Build dependencies

Show dependencies

``` shell
# Show first-level of dependencies
rebar3 deps

# Show all dependencies
rebar3 tree
```

Override library

``` shell
# Clone a dependency into a folder
_checkouts/<repo>
```

### Erlang tips'n'tricks

``` shell
# Get Erlang version
erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt(0)'

# Show loaded modules
erlang:loaded().

# Get running applications
application:which_applications().

# Show processes
regs().
rp(registered()).

help().

# Show modules exports and more
m(modulename).

# Debug print Erlang terms, linebreak at 80chars
io:format("DEBUG: ~p~n", [Data]),

# Start a graphical tool for observing characteristics of Erlang systems
observer:start().
```

Rebar, use something like
``` shell
{erl_opts, [debug_info, warn_missing_spec, warnings_as_errors]}.
```

### Code formatting
See http://tech.nextroll.com/blog/dev/2020/02/25/erlang-rebar3-format.html

## Links

* [Rebar3 howto](https://praglowski.com/2016/05/10/your-first-erlang-application-with-rebar3)
* [Analyse using GDB](https://www.erlang-solutions.com/blog/how-to-analyse-a-beam-core-dump.html)
* [Guidelines](https://github.com/inaka/erlang_guidelines)
