# Erlang hacks

## Projects
* frogger - A frogger game in Erlang
* redis-usage - Test of eredis
* hex - binary to/from hex converter functions

## Setup

### Install erlang

``` bash
# See https://github.com/kerl/kerl
brew install kerl
kerl build 22.2
kerl install 22.2 ~/erlang/22.2
. ~/erlang/22.2/activate

# Install rebar3
curl -L https://github.com/erlang/rebar3/releases/download/3.13.0/rebar3 -o ~/bin/rebar3 && chmod +x ~/bin/rebar3

# Install elvis
cd /tmp; git clone https://github.com/inaka/elvis.git; cd elvis; rebar3 escriptize; cp _build/default/bin/elvis ~/erlang/22.2/bin/erl
```

### Create new project

``` bash
rebar3 new app <name>
```

### Run

``` shell
# Style reviewer
elvis rock

# Run
rebar3 shell
```

### Other

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
```

Rebar, use something like
``` shell
{erl_opts, [debug_info, warn_missing_spec, warnings_as_errors]}.
```

Override library
``` shell
# Clone a dependency into a folder
_checkouts/<repo>
```

## Links

[Rebar3 howto](https://praglowski.com/2016/05/10/your-first-erlang-application-with-rebar3)
[Analyse using GDB](https://www.erlang-solutions.com/blog/how-to-analyse-a-beam-core-dump.html)
