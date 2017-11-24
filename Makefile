JS_PRIV=apps/xprof_gui/priv
BIN_DIR:=node_modules/.bin
NIF_DIR:=apps/xprof_nif

compile: nif
	./rebar3 compile

dev: webpack nif
	./rebar3 as dev compile, shell

npm:
	cd $(JS_PRIV); npm install

bootstrap_front_end: npm

check_front_end:
	cd $(JS_PRIV); $(BIN_DIR)/eslint *.json app/*.jsx app/*.js test/*.js test/*.jsx

test_front_end: check_front_end
	cd $(JS_PRIV); $(BIN_DIR)/mocha test/.setup.js test/*.test.js test/*.test.jsx

webpack: test_front_end
	cd $(JS_PRIV); $(BIN_DIR)/webpack -d

webpack_autoreload: npm
	cd $(JS_PRIV); $(BIN_DIR)/webpack -w -d

nif:
	gcc -I /usr/lib/erlang/usr/include/ \
		  -fPIC -shared \
			-o $(NIF_DIR)/xprof_core_nif_tracer.so \
			$(NIF_DIR)/xprof_core_nif_tracer.c

test: compile
	./rebar3 do eunit -c, ct -c, cover

doc:
	./rebar3 edoc

dialyzer:
	./rebar3 dialyzer

.PHONY: compile dev npm bootstrap_front_end check_front_end test_front_end webpack webpack_autoreload test doc dialyzer
