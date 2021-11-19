.. _`installation`:

##############################
Installation
##############################

Use the following to get the latest code::

    git clone git://git.code.sf.net/p/stingrayreader/code stingrayreader-code
   
The alternative is to use PIP::

    python -m pip install stingray-reader

Development
=============================

The following tools are used for development

-   Sphinx.  http://sphinx.pocoo.org/

-   pytest.

-   pytest-cov.

-   tox.

   
Installation via Distutils
=============================
   
To install ``stingray`` you can use the following.

..  code-block:: bash

    sudo python setup.py install
   
In some cases, you might want to break this down into a build step that
doesn't require privileges and the final install step, which does require
privileges.

..      code-block:: bash
       
        python setup.py build
        sudo python setup.py install
