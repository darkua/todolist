.PHONY:

all:
	./rebar get-deps compile

clean:
	./rebar clean
	rm -rf apps/*/deps
	rm -rf deps/*/deps
	rm -rf apps/*/doc/*.html
	rm -rf apps/*/doc/*.png
	rm -rf apps/*/doc/*.css
	rm -rf apps/*/doc/edoc-info
	rm -rf deps/*/doc/*html
	rm -rf apps/*/logs
	rm -rf deps/*/logs
	find . -name "*~" -exec rm {} \;
	find . -name ".#*" -exec rm {} \;
	find . -name "erl_crash.dump" -exec rm {} \;
	find . -name "#*#" -exec rm {} \;

doc:
	@erl -pa apps/*/ebin -pa deps/*/ebin -noshell -sname fraud_path -eval 'http_doc:do(fun fraud_api_app:appmods/0), init:stop(0).'
	./rebar doc skip_deps=true

blank:
	cd rel && rm -rf fraud*; cd ..
	./rebar delete-deps clean

app:
	./rebar compile skip_deps=true

ct:
	rm -rf apps/*/logs/
	./rebar ct skip_deps=true

ct_case:
	rm -rf apps/*/logs/
	./rebar ct skip_deps=true suites=${SUITE} case=${CASE}

test:
	rm -rf .eunit
	./rebar eunit skip_deps=true

release:
	make clean
	make
	./rebar generate

upgrade:
	make clean
	make
	rm -rf deps/yaws/rel
	./rebar generate overlay_vars=${NODE}.config
	./rebar generate-appup previous_release=${PRE}
	./rebar generate-upgrade previous_release=${PRE}
	cd rel && mv fraud_*.tar.gz ${PRE}/releases/; cd ..

## add your dependecies here. --apps [depencencies from otp] -r [our deps]
## fixme statebox and yaws doesn't work with dialyzer right now.
init_dialyzer:
	rm -rf deps/yaws/ebin/*beam
	rm -rf deps/riak_pb/ebin/*beam
	rm -rf deps/statebox/ebin/*beam
	dialyzer --apps ssl xmerl stdlib kernel inets crypto public_key -r deps --build_plt --output_plt dialyzer.plt

dialyzer:
	rm -rf apps/*/.eunit
	dialyzer -r apps --plt dialyzer.plt

help:
	@echo "Commands:"
	@echo "  Build:  \t make"
	@echo "  Test:   \t make test"
	@echo "  Release:\t make release NODE=<name>"
	@echo "          \t - name should match a file in rel/<name>.config"
	@echo "  Upgrade:\t make upgrade_rel NODE=<name> PRE=<previous_rel>"
	@echo "          \t - name should match a file in rel/<name>.config"
	@echo "          \t - previous_rel should match a directory under rel/"
