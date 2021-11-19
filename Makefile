# Stingray Reader
.PHONY: test docs

test:
	tox

docs:
	cd docs && PYTHONPATH=$(pwd).. $(MAKE) html
