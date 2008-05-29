SHELL=/bin/sh

EFLAGS=-pa ebin

.PHONY: tgz

all: lock_serv.boot

tgz: lock_serv.tar.gz

test: ebins
	erl $(EFLAGS) -run lock_serv_test -run init stop
	open cov.html

ebins:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make
	cp src/*.app ebin

lock_serv.boot: ebins lock_serv.rel
	erlc -W -v $(EFLAGS) lock_serv.rel

lock_serv.tar.gz: lock_serv.boot
	erl $(EFLAGS) -noshell -run systools make_tar lock_serv -run init stop

clean:
	rm -f lock_serv.beam lock_serv.boot lock_serv.script lock_serv.tar.gz
	rm -rf ebin
