0.0000 01  COUNTY-CROSS-REFERENCE-RECORD.
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

0.0000 01  COPYRIGHT-HEADER-RECORD.
            05  FILLER                                     PIC  X(05).
            05  FILE-VERSION-YEAR                          PIC  X(02).
            05  FILE-VERSION-MONTH                         PIC  X(02).
            05  COPYRIGHT-SYMBOL                           PIC  X(11).
            05  TAPE-SEQUENCE-NO                           PIC  X(03).
            05  FILLER                                     PIC  X(30).
