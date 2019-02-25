
all: patch
	rebar3 compile


clean:
	rm -rf _build
	rm -rf _checkouts
	rm -rf rebar.lock

patch: _checkouts/eredis_pool
	@sed -i "s/{ok, Pools} = application:get_env(eredis_pool, pools)/Pools = application:get_env(eredis_pool, pools, [])/" _checkouts/eredis_pool/src/eredis_pool_sup.erl
	@sed -i "s/{ok, GlobalOrLocal} = application:get_env(eredis_pool, global_or_local)/GlobalOrLocal = application:get_env(eredis_pool, global_or_local, local)/" _checkouts/eredis_pool/src/eredis_pool_sup.erl

_checkouts/eredis_pool:
	git clone https://github.com/hiroeorz/eredis_pool.git _checkouts/eredis_pool

run: all
	rebar3 shell --apps redisapp
