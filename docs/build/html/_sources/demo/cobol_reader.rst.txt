..  _`demo_cobol`:

##########################################################
Reading COBOL Files
##########################################################

For sample data, we're using data found here:
http://wonder.cdc.gov/wonder/sci_data/codes/fips/type_txt/cntyxref.asp

The data files are in two zip archives: http://wonder.cdc.gov/wonder/sci_data/datasets/zipctyA.zip 
and http://wonder.cdc.gov/wonder/sci_data/datasets/zipctyB.zip

Each of these archives contains five large files, with 2,310,000 rows of data, plus a header. 
The 10th file has 2,037,944 rows of data plus a header.

The member names of the ZIP archive are zipcty1 to zipcty5
and zipcty6 to zipcty10.

We'll work with two small subsets in the sample directory.

Here are the two record layouts.

..    parsed-literal::

    COUNTY CROSS-REFERENCE FILE - COBOL EXAMPLE

            BLOCK CONTAINS 0 RECORDS
            LABEL RECORDS ARE STANDARD
            RECORD CONTAINS 53 CHARACTERS
            RECORDING MODE IS F
            DATA RECORDS ARE
                   COUNTY-CROSS-REFERENCE-RECORD.

        01  COUNTY-CROSS-REFERENCE-RECORD.
            05   ZIP-CODE                                 PIC X(05).
            05   UPDATE-KEY-NO                            PIC X(10).
            05   ZIP-ADD-ON-RANGE.
                 10  ZIP-ADD-ON-LOW-NO.
                      15  ZIP-SECTOR-NO                   PIC X(02).
                      15  ZIP-SEGMENT-NO                  PIC X(02).
                 10  ZIP-ADD-ON-HIGH-NO.
                      15  ZIP-SECTOR-NO                   PIC X(02).
                      15  ZIP-SEGMENT-NO                  PIC X(02).
            05   STATE-ABBREV                             PIC X(02).
            05   COUNTY-NO                                PIC X(03).
            05   COUNTY-NAME                              PIC X(25).

..    parsed-literal::

    COPYRIGHT HEADER RECORD - COBOL EXAMPLE

               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 53 CHARACTERS
               RECORDING MODE IS F
               DATA RECORDS ARE
                   COPYRIGHT-HEADER RECORD.

          01  COPYRIGHT-HEADER-RECORD.
              05  FILLER                                     PIC  X(05).
              05  FILE-VERSION-YEAR                          PIC  X(02).
              05  FILE-VERSION-MONTH                         PIC  X(02).
              05  COPYRIGHT-SYMBOL                           PIC  X(11).
              05  TAPE-SEQUENCE-NO                           PIC  X(03).
              05  FILLER                                     PIC  X(30).

Implementation
==============

The actual COBOL code is in :file:`sample/zipcty.cob`.
This file has both record layouts.
These are two ``01`` level items in a single file.
    
When working with unknown files, we sometimes need to preview a raw dump of the
records.

..  literalinclude:: ../../../demo/cobol_reader.py
    :lines: 89-91

This is a handy expedient for debugging.

As suggested in :ref:`developer`, here are two builder functions.
The ``header_builder()`` function creates a header object from the 
first row of each :file:`zipcty*` file. 

..  literalinclude:: ../../../demo/cobol_reader.py
    :lines: 94-100

  
The ``detail_builder()`` function creates a detail object from the subsequent
rows of each :file:`zipcty*` file. 

Because the names within the COBOL layout are not unique at the bottom-most
element level, we must use path names. The default path names include all
levels of the DDE. More clever path name components might be useful here.

COBOL uses an "of" to work up the hierarchy looking for a unique name.

Maybe we could build a fluent interface ``schema['ZIP-SECTOR-NO'].of('ZIP-ADD-ON-LOW-NO')``.

..  literalinclude:: ../../../demo/cobol_reader.py
    :lines: 110-129

Here's the ``process_sheet()`` function which applies the builders to the various
rows in each sheet. Currently, all that happens is a print of the object that
was built.

Note that we've transformed the schema from a simple, flat list into
a dictionary keyed by field name. For COBOL processing, this is essential, since
the numeric order of fields isn't often sensible.

Also note that we've put two versions of each name into the schema dictionary.

-   The lowest level name.

-   The entire path down to the lowest level name.

[For spreadsheets, where columns are numbered, the positional information may be
useful.]

..  literalinclude:: ../../../demo/cobol_reader.py
    :lines: 132-150

The top-level script must do two things:

1.  Parse the :file:`"zipcty.cob"` data definition to create a schema.

2.  Open a data file as a :py:class:`cobol.Character_File`. This presumes
    the file is all character (no COMP-3) and already translated into ASCII.
    
    The :py:func:`process_sheet` is applied to each file.

Here's a function to parse arguments.

..  literalinclude:: ../../../demo/cobol_reader.py
    :lines: 156-169

Given this function to parse the command-lines
arguments, the ``main()`` function looks
like this:

..  literalinclude:: ../../../demo/cobol_reader.py
    :lines: 172-192


Running the demo
================

We can run this program like this:

..  code-block:: bash

    python demo/cobol_reader.py --schema sample/zipcty.cob sample/zipcty[1-2]

The output looks like this.

..  parsed-literal::

    {'file_version_year': '88', 'file_version_month': '09', 'copyright_symbol': ' (C)USPS   ', 'tape_sequence_no': '001'}
    {'zip_code': '00401', 'update_key_no': '0000000001', 'low_sector': '00', 'low_segment': '01', 'high_sector': '00', 'high_segment': '01', 'state_abbrev': 'NY', 'county_no': '119', 'county_name': 'WESTCHESTER              '}
    {'zip_code': '02186', 'update_key_no': '0000462001', 'low_sector': '52', 'low_segment': '66', 'high_sector': '52', 'high_segment': '66', 'state_abbrev': 'MA', 'county_no': '021', 'county_name': 'NORFOLK                  '}
    {'zip_code': '06111', 'update_key_no': '0000924001', 'low_sector': '49', 'low_segment': '01', 'high_sector': '49', 'high_segment': '01', 'state_abbrev': 'CT', 'county_no': '003', 'county_name': 'HARTFORD                 '}
    {'zip_code': '07901', 'update_key_no': '0001386001', 'low_sector': '22', 'low_segment': '08', 'high_sector': '22', 'high_segment': '08', 'state_abbrev': 'NJ', 'county_no': '039', 'county_name': 'UNION                    '}
    {'zip_code': '10463', 'update_key_no': '0001848001', 'low_sector': '17', 'low_segment': '05', 'high_sector': '17', 'high_segment': '05', 'state_abbrev': 'NY', 'county_no': '005', 'county_name': 'BRONX                    '}
    INFO:__main__:Counter({'read': 5})
    {'file_version_year': '88', 'file_version_month': '09', 'copyright_symbol': ' (C)USPS   ', 'tape_sequence_no': '002'}
    {'zip_code': '11789', 'update_key_no': '0002310001', 'low_sector': '25', 'low_segment': '43', 'high_sector': '25', 'high_segment': '43', 'state_abbrev': 'NY', 'county_no': '103', 'county_name': 'SUFFOLK                  '}
    {'zip_code': '14767', 'update_key_no': '0002772001', 'low_sector': '97', 'low_segment': '71', 'high_sector': '97', 'high_segment': '71', 'state_abbrev': 'NY', 'county_no': '013', 'county_name': 'CHAUTAUQUA               '}
    {'zip_code': '17201', 'update_key_no': '0003234001', 'low_sector': '90', 'low_segment': '33', 'high_sector': '90', 'high_segment': '33', 'state_abbrev': 'PA', 'county_no': '055', 'county_name': 'FRANKLIN                 '}
    {'zip_code': '19438', 'update_key_no': '0003696001', 'low_sector': '28', 'low_segment': '22', 'high_sector': '28', 'high_segment': '22', 'state_abbrev': 'PA', 'county_no': '091', 'county_name': 'MONTGOMERY               '}
    {'zip_code': '21740', 'update_key_no': '0004158001', 'low_sector': '53', 'low_segment': '05', 'high_sector': '53', 'high_segment': '05', 'state_abbrev': 'MD', 'county_no': '043', 'county_name': 'WASHINGTON               '}


Working with Archives
=====================

We don't need to unpack the archives to work with
files inside them.
We can open a ZipFile member and process that. 
This can be a helpful optimization when small extracts are pulled from ZIP archives.

The trick is this.

When we open the file with ``COBOL_Text_File(filename)``
we can pass the file object created by ``ZipFile.open()`` as the second argument.

It looks like this::

    with COBOL_Text_File(filename, file_object=archive.open(filename)) as wb:
        ...

This uses an already opened explicit file object rather
than opening the given file name.


