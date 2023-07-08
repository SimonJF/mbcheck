all:
	@if dune build; then\
		ln -sf _build/default/bin/main.exe mbcheck;\
	fi

.PHONY: test
test:
	cd test && python3 run-tests.py

.PHONY: pin
pin:
	opam pin add . --kind=path -y

.PHONY: install
install:
	opam reinstall mbcheck

.PHONY: uninstall
uninstall:
	opam remove mbcheck

.PHONY: clean
clean:
	dune clean
	rm -f mbcheck
