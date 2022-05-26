# Stingray Reader
.PHONY: test docs

test:
	tox --skip-missing-interpreters

docs:
	cd docs && PYTHONPATH=$(pwd).. $(MAKE) html

docs-coverage:
	cd docs && PYTHONPATH=$(pwd).. SPHINXOPTS="-b coverage" $(MAKE) html

apidoc_gen:
	PYTHONPATH=$(pwd) sphinx-apidoc --separate -o apidoc stingray
