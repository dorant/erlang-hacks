# Erlang hacks

# Setup

## Install erlang

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

## Create new project

``` bash
rebar3 new app <name>
```

## Run

``` shell
# Style reviewer
elvis rock

# Run
rebar3 shell
```

# Projects
* frogger - A frogger game in Erlang
* redis-usage - Test of eredis
* hex - binary to/from hex converter functions
