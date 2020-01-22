# Erlang hacks

# Setup

## Install erlang

``` bash
# See https://github.com/kerl/kerl
brew install kerl
kerl build 22.2
kerl install 22.2 ~/erlang/22.2
. ~/erlang/22.2/activate

curl -L https://github.com/erlang/rebar3/releases/download/3.13.0/rebar3 -o ~/bin/rebar3 && chmod +x ~/bin/rebar3
```

## Create new project

``` bash
rebar3 new app <name>
```

## Run

``` shell
rebar3 shell
```

# Projects
* frogger - A frogger game in Erlang
* redis-usage - Test of eredis
* hex - binary to/from hex converter functions
