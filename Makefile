SHELL=/bin/sh

EFLAGS=-pa ebin

.PHONY: tgz

all: lock_supervisor.boot

tgz: lock_serv.tar.gz

test: ebins
	erl $(EFLAGS) -run lock_serv_test -run init stop
	open cov.html

ebins:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make
	cp src/*.app ebin

lock_supervisor.boot: ebins lock_supervisor.rel
	erlc -W -v $(EFLAGS) lock_supervisor.rel

lock_serv.tar.gz: lock_serv.boot
	erl $(EFLAGS) -noshell -run systools make_tar lock_supervisor -run init stop

clean:
	rm -f lock_supervisor.beam lock_supervisor.boot lock_supervisor.script
	rm -f lock_supervisor.tar.gz
	rm -f cov.html erl_crash.dump
	rm -rf ebin
