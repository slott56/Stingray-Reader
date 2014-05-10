version=4.3.4
# sftp -oIdentityFile=~/.ssh/id_dsa
(cd build/html
sftp slott56@web.sourceforge.net <<END
cd /home/project-web/stingrayreader/htdocs
put *
put _static/* _static
put _source/* _source
put _images/* _images
put _images/math/* _images/math
put demo/* demo
put testing/* testing
bye
END
)
(cd build/latex
sftp slott56@web.sourceforge.net <<END
cd /home/pfs/project/s/st/stingrayreader
put stingray.pdf stingray.pdf
bye
END
)
sftp slott56@web.sourceforge.net <<END
cd /home/pfs/project/s/st/stingrayreader
put README.rst README.rst
bye
END
zip -r stingray-${version}.zip  build.py  README.rst setup.py source stingray test demo sample build/html
sftp slott56@web.sourceforge.net <<END
cd /home/pfs/project/s/st/stingrayreader
put stingray-${version}.zip stingray-${version}.zip
bye
END
