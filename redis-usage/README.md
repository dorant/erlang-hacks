

# Test of eredis

## Prepare
```
brew install erlang
brew install rebar3
```

## Dependencies
```
git clone https://github.com/hiroeorz/eredis_pool.git _checkouts/eredis_pool
```


```
# Build
rebar3 compile

# Start
rebar3 shell
application:start(redisapp).
application:stop(redisapp).

```


#### Some rebar3 usage
```
# Howto setup and application
rebar3 new app redisapp

# Auto-download dependencies
Add to rebar.config:  {plugins, [rebar3_hex]}.
rebar3 update
# Download and build
rebar3 compile
```
