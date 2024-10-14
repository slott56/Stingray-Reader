# Stingray Reader
.PHONY: test docs docs-coverage apidoc_gen

test:
	tox run

quick:
	tox run -e quick

docs:
	PYTHONPATH=src python -m doctest docs/source/developer.rst
	cd docs && $(MAKE) html

docs-coverage:
	cd docs && SPHINXOPTS="-b coverage" $(MAKE) html

apidoc_gen:
	PYTHONPATH=$(pwd) sphinx-apidoc --separate -o apidoc stingray

install-plantuml:
	-mkdir .plantuml
	curl -LSf https://github.com/plantuml/plantuml/releases/download/v1.2024.7/plantuml-1.2024.7.jar -o .plantuml/plantuml-1.2024.7.jar
