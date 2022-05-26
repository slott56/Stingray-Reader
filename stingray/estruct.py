"""
estruct -- Unpack bytes with EBCDIC encodings

The :py:mod:`estruct` module unpacks EBCDIC-encoded values. It is a big-endian version of the :py:mod:`struct` module. 
It uses two COBOL DDE clauses, ``USAGE`` and ``PIC``, to describe the format of data represented by a sequence of bytes.

Unpacking and Sizing
====================

The format string is a COBOL DDE. The ``USAGE`` and ``PIC`` (also spelled ``PICTURE``) clauses are required,
the rest of the DDE is quietly ignored.
For example, ``'USAGE DISPLAY PIC S999.99'``, is the minimum to describe a textual value that occupies
7 bytes.

The :py:func:`unpack` uses the format string to unpack bytes into useful Python values.
As with the built-in :py:func:`struct.unpack`, the result is **always** a tuple even if
it has a single value.

The :py:func:`calcsize` functions uses the format string to compute the size of a value.
This can be applied to a DDE to compute the offsets and positions of each field.

..  note:: Alternative Format Strings

    The :py:mod:`struct` module uses a compact format string describe data.
    This string is used unpack text, int, and float values from a sequence of bytes.
    See https://docs.python.org/3/library/struct.html#format-characters.
    An alternative interface for this module could be to use single-letter codes.
    
    For example:
    
    -   ``15x`` for display. 
    -   ``f`` and ``d`` for COMP-1 and COMP-2. 
    -   ``9.2p`` for ``PIC 9(9)V99`` packed decimal COMP-3. 
    -   ``9.2n`` for zoned decimal text, DISPLAY instead of computational.
    -   ``h``, ``i``, and ``l`` for COMP-4 variants.
     
     This seems needless, but it is compact and somewhat more compatible with the :py:mod:`struct` module.

Examples::

    >>> import stingray.estruct
    >>> stingray.estruct.unpack("USAGE DISPLAY PIC S999V99", ' 12345'.encode("cp037"))
    (Decimal('123.45'),)
    >>> stingray.estruct.unpack("USAGE DISPLAY PIC X(5)", 'ABCDE'.encode("cp037"))
    ('ABCDE',)
    >>> stingray.estruct.calcsize("USAGE COMP-3 PIC S9(11)V9(2)")
    7

File Reading
============

An EBCDIC file can leverage physical "Record Format" (RECFM) assistance.
These classes define a number of Z/OS RECFM conversion functions. We recognize four
actual RECFM's plus an additional special case.

-   ``F`` - Fixed.  :py:class:`RECFM_F`

-   ``FB`` - Fixed Blocked.  :py:class:`RECFM_FB`

-   ``V`` - Variable, each record is preceded by a 4-byte Record Description Word (RDW).
    :py:class:`RECFM_V`

-   ``VB`` - Variable Blocked. Blocks have Block Description Word (BDW); each record within a block has a Record Description Word.
    :py:class:`RECFM_VB`

-   ``N`` - Variable, but without BDW or RDW words. This involves some buffer management
    magic to recover the records properly. This is required to handle ``Occurs Depending On`` cases
    where there's no V or VB header. This requires the consumer of bytes to announce how many bytes
    were consumed so the reader can advance an appropriate amount.
    :py:class:`RECFM_N`

Each of these has a :py:meth:`RECFM_Reader.record_iter` iterator that emits records stripped of header word(s).

::

    with some_path.open('rb') as source:
        for record in RECFM_FB(source, lrecl=80).record_iter():
            process(record)

..  note::  IBM z/Architecture mainframes are all big-endian

COBOL Picture Parsing
=====================

The :py:class:`Representation` object provides representation details
based on COBOL syntax. This is used by the Struct Unpacker (:py:class:`schema_instance.Struct`) as well as the
EBCDIC Unpacker (:py:class:`schema_instance.EBCDIC`).

In principle, this might be a separate thing, or might be part of the :py:mod:`cobol_parser` module.
For now, it's here and is reused by :py:mod:`schema_instance`.
"""

import abc
from collections.abc import Iterator
import logging
from typing import Any, NamedTuple, Type, Optional, TextIO, BinaryIO
import re
from decimal import Decimal
import struct

logger = logging.getLogger("stingray.estruct")


class DesignError(BaseException):
    """
    This is a catastrophic design problem.
    A common root cause is a named REGEX capture clause that's not properly
    handled by a class, method, or function.
    """
    pass


#: Pattern for parsing COBOL ``USAGE`` and ``PICTURE`` clauses.
clause_pattern = re.compile(
    r"(?:USAGE\s+)?(?:IS\s+)?"
        r"(?P<usage>BINARY|COMPUTATIONAL-1|COMPUTATIONAL-2|COMPUTATIONAL-3|COMPUTATIONAL-4|"
        r"COMPUTATIONAL|COMP-1|COMP-2|COMP-3|COMP-4|COMP|DISPLAY|PACKED-DECIMAL)"
    r"|(?:PIC|PICTURE)\s+(?:IS\s+)?(?P<picture>\S+)"
)


class Representation(NamedTuple):
    """COBOL Representation Details: Usage and Picture.

    This is used internally by :py:func:`unpack` and :py:func:`calcsize`.
    
    >>> r = Representation.parse("USAGE DISPLAY PICTURE S9(5)V99")
    >>> r
    Representation(usage='DISPLAY', picture_elements=[{'sign': 'S'}, {'digit': '99999'}, {'decimal': 'V'}, {'digit': '99'}], picture_size=8)
    >>> r.pattern
    '[ +-]?\\\\d\\\\d\\\\d\\\\d\\\\d\\\\d\\\\d'
    >>> r.digit_groups
    ['S', '99999', 'V', '99']
    """

    #: The usage text, words like ``DISPLAY`` or ``COMPUTATIONAL`` or any of the numerous variants.
    usage: str

    #: The decomposed ``PIC`` clause, created by the :py:meth:`normalize_picture` method.
    picture_elements: list[dict[str, str]]

    #: Summary sizing information.
    picture_size: int

    @staticmethod
    def normalize_picture(source: str) -> list[dict[str, str]]:
        """
        Normalizes the ``PIC`` clause into a sequence of component details.
        This extracts ``sign``, editing characters in ``char``, the decimal place in ``decimal``,
        any repeated picture characters with ``x(n)``, and any non-repeat-count picture
        characters.

        The repeat count items are normalized into non-repeat-count. ``9(5)`` becomes ``99999``.

        :param source: The string value of a `PICTURE` clause
        :returns: a list of dictionaries that decomposes the picture
        """
        pic_pattern = re.compile(
            r"(?P<sign>\+|-|S|DB|CR)"
            r"|(?P<char>\$|,|/|\*|B)"
            r"|(?P<decimal>V|\.)"
            r"|(?P<repeat>[AX9Z0]\(\d+\))"
            r"|(?P<digit>[AX9Z0]+)"
        )
        picture: list[dict[str, str]] = []
        ending = -1
        picture_elements = pic_pattern.finditer(source)
        for pic in picture_elements:
            ending = pic.end()
            if pic.groupdict()["repeat"]:
                # Normalize Picture: expand 9(5) to {"digit": "99999"}
                # Mypy doesn't like the assignment below.
                # It's not clear the value of pic.groupdict()["repeat"] will be
                # large enough to support 4 or more substrings.
                count: str
                char: str
                char, left, *count, right = pic.groupdict()["repeat"]  # type: ignore [misc]
                picture.append({"digit": int("".join(count)) * char})
            else:
                # No need to normalize, use {"digit": "99999"} or whatever we found 
                non_empty = {n: v for n, v in pic.groupdict().items() if v}
                picture.append(non_empty)
        if ending != len(source):
            raise ValueError(
                f"Invalid characters {source[ending:]!r} in PIC {source!r}"
            )
        return picture

    @classmethod
    def parse(cls: Type["Representation"], format: str) -> "Representation":
        """
        Parse the COBOL DDE information. Extract the ``USAGE`` and ``PICTURE`` details
        to create a ``Representation`` object.

        :param cls: the class being created a subclass of :py:class:`Representation`
        :param format: the format specification string
        :returns: An instance of the requested class.
        
        ..  todo:: Add constraints in picture rules.
        """
        usage = "DISPLAY"
        pic_source = ""
        picture: list[dict[str, str]] = []
        size = 0

        # Extract the USAGE and PICTURE clauses from the COBOL
        clause_iter = clause_pattern.finditer(format)
        for c in clause_iter:
            if c.groupdict()["usage"]:
                usage = c.groupdict()["usage"]
            elif c.groupdict()["picture"]:
                pic_source = c.groupdict()["picture"]
                picture = cls.normalize_picture(pic_source)
    
        # Compute the size of the PICTURE
        for elt in picture:
            if sign := elt.get("sign", ""):
                size += len(sign)
            elif char := elt.get("char", ""):
                size += len(char)
            elif decimal := elt.get("decimal", ""):
                size += 1 if decimal == "." else 0
            elif digits := elt.get("digit", ""):
                # A, X, Z, 9, 0 count.
                # P doesn't count.
                size += len(digits)
            elif elt.get("repeat"):  # pragma: no cover
                # The PICTURE clause was not normalized.
                raise DesignError(f"failure to normalize picture {picture!r}")
            else:  # pragma: no cover
                # The regex pattern and the if statement don't match.
                raise DesignError(f"unexpected {elt} in picture {picture!r}")
        return cls(usage, picture, size)

    @property
    def pattern(self) -> str:
        """
        Summarize Picture Clause as a regexp to validate data.
        
        .. todo:: This is too flexible: sign and decimal should not be allowed to repeat.
        """
        regexp: list[str] = []
        for elt in self.picture_elements:
            if sign := elt.get("sign", ""):
                regexp += sign.replace("S", "[ +-]?").replace("s", "[ +-]?")
            elif char := elt.get("char", ""):
                regexp += (
                    char.replace("$", "\\$").replace("*", "\\*").replace("B", "\\s")
                )
            elif decimal := elt.get("decimal", ""):
                regexp += "\\." if decimal == "." else ""
            elif digits := elt.get("digit", ""):
                # A, X, Z, 9, 0 count.
                # P doesn't count.
                regexp += (
                    digits.replace("A", "\\w")
                    .replace("X", ".")
                    .replace("Z", "\\d")
                    .replace("9", "\\d")
                    .replace("0", "\\d")
                    .replace("P", "")
                )
            elif elt.get("repeat"):  # pragma: no cover
                # The PICTURE clause was not normalized.
                raise DesignError(f"failure to normalize picture {self.picture_elements!r}")
            else:  # pragma: no cover
                # The regex pattern and the if statement don't match.
                raise DesignError(f"unexpected {elt} in picture {self.picture_elements!r}")
        return "".join(regexp)
    
    @property
    def digit_groups(self) -> list[str]:
        """Parse the Picture into details: [sign, whole, separator, fraction] groups."""
        group = 1  # whole number part
        groups = ["", "", "", ""]
        for d in self.picture_elements:
            if decimal := d.get("decimal"):
                groups[2] = decimal
                group = 3  # decimal fraction part
            elif digit_chars := d.get("digit"):
                groups[group] += digit_chars
            elif edit_chars := d.get("char"):
                groups[group] += edit_chars.count("*")*"9"  # The "*"'s count as digits
            elif sign_char := d.get("sign"):
                groups[0] = sign_char
        return groups

    @property
    def zoned_decimal(self) -> bool:
        """Examine the digit groups to see if this is purely numeric."""
        edit_options: list[str] = list(filter(None, (x.get("char") for x in self.picture_elements)))
        numeric_options = (
            self.picture_size != 0,  # If PIC omitted, size will be zero, and it's a GROUP element.
            self.digit_groups[0] in {"", "S", "s", None},
            all(d == "9" for d in self.digit_groups[1]),
            self.digit_groups[2] in {"V", "v", "", None},
            all(d == "9" for d in self.digit_groups[3]),
        )
        zoned_decimal = all(numeric_options) and not edit_options
        logger.debug(
            f"PIC {self.digit_groups}: all({numeric_options=}) and not {edit_options=} = {zoned_decimal=}"
        )
        return zoned_decimal

def unpack(format: str, buffer: bytes) -> tuple[Any, ...]:
    """
    Unpack EBCDIC bytes given a COBOL DDE format specification and a buffer of bytes.
    
    USAGE DISPLAY special case: "external decimal" sometimes called "zoned decimal".
    The PICTURE character-string of an external decimal item can contain only:

    - One or more of the symbol 9

    - The operational-sign, S

    - The assumed decimal point, V

    - One or more of the symbol P
        
    External decimal items with USAGE DISPLAY are sometimes referred to as zoned decimal items. 
    Each digit of a number is represented by a single byte. 
    The 4 high-order bits of each byte are zone bits; 
    the 4 high-order bits of the low-order byte represent the sign of the item. 
    The 4 low-order bits of each byte contain the value of the digit.

    .. todo:: Add support for COMP-1 and COMP-2.

    :param format: A format string; a COBOL DDE.
    :param buffer: A bytes object with a value to be unpacked.
    :return: A Python object
    """
    representation = Representation.parse(format)

    # Usage Display can be text or zoned decimal
    if representation.usage in ("DISPLAY",):
        # Depends on PIC. If PIC has only S9VP, then it's "ZONED DECIMAL": a number.
        # Otherwise, it's actually text
        if representation.zoned_decimal:
            text = "".join(str(b & 0x0F) for b in buffer)
            sign_half = (buffer[-1] & 0xF0) >> 4
            sign = -1 if (sign_half == 0x0B or sign_half == 0x0D) else +1
            base = Decimal(text)
            scale = Decimal(10) ** (-len(representation.digit_groups[3]))
            return base * scale * sign,
        # Match text with a regular expression derived from the picture to see if it's valid.
        text = buffer.decode("CP037")
        logger.debug(f"estruct.unpack: {buffer!r} == {text=}")
        if not re.match(representation.pattern, text):
            raise ValueError(f"{text!r} doesn't match pattern {representation.pattern!r}")
        # It's display text.
        return text,

    # Usage COMP-3
    elif representation.usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL",):
        # unpack buffer bytes into pairs of digits
        half_bytes = []
        for b in buffer:
            half_bytes.append((b & 0xF0) >> 4)
            half_bytes.append((b & 0x0F))
        *digits, sign_half = half_bytes
        # get sign and base numeric value
        sign = -1 if (sign_half == 0x0B or sign_half == 0x0D) else +1
        base = Decimal("".join(str(d) for d in digits))
        scale = Decimal(10) ** (-len(representation.digit_groups[3]))
        return base * scale * sign,

    # elif representation.usage in ("COMPUTATIONAL-1", "COMP-1")
    #   Unpack 4 byte float
    #   return struct.unpack(">f", buffer)

    # elif representation.usage in ("COMPUTATIONAL-2", "COMP-2")
    #   Unpack 8 byte float
    #   return struct.unpack(">d", buffer)

    elif representation.usage in ("COMP-4", "COMPUTATIONAL-4", "BINARY", "COMP", "COMPUTATIONAL",):
        # Unpack 2-, 4-, or 8-byte int. Size depends on the length of digit_groups[1]
        # 1-4 digits is two bytes. 5-9 digits is 4 bytes. 10-18 is 8 bytes.
        if len(representation.digit_groups[1]) < 5:
            format = ">h"
        elif 5 <= len(representation.digit_groups[1]) < 10:
            format = ">i"
        elif 10 <= len(representation.digit_groups[1]) < 18:
            format = ">q"
        else:  # pragma: no cover
            raise ValueError(f"Usage {representation!r} too large")
        n = struct.unpack(format, buffer)
        return n

    else:  # pragma: no cover
        raise RuntimeError(f"Usage {representation.usage!r} not supported")


def calcsize(format: str) -> int:
    """
    Compute the size, in bytes for an elementary (non-group-level) COBOL DDE format specification.

    :param format: The COBOL ``DISPLAY`` and ``PIC`` clauses.
    :returns: integer size of the item in bytes.
    """
    representation = Representation.parse(format)

    if representation.picture_size == 0:
        raise ValueError(f"Problem parsing {format!r}")

    if representation.usage in ("DISPLAY",):
        return representation.picture_size
    elif representation.usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL",):
        return (representation.picture_size + 1) // 2
    elif representation.usage in ("COMP-1", "COMPUTATIONAL-1",):
        return 4
    elif representation.usage in ("COMP-2", "COMPUTATIONAL-2",):
        return 8
    elif representation.usage in ("COMP-4", "COMPUTATIONAL-4", "BINARY", "COMP", "COMPUTATIONAL",):
        return 2 if representation.picture_size < 5 else (4 if 5 <= representation.picture_size < 10 else 8)
    else:
        # This is a design error: regular expression doesn't match this if statement
        raise RuntimeError(
            f"Unparsable {representation.usage} in picture: {format!r}"
        )  # pragma: no cover


class RECFM_Reader(abc.ABC):
    """
    Reads records based on a physical file format.

    A subclass can handle details of the various kinds of Block and Record
    Descriptor Words (BDW, RDW) present a specific format.
    """

    def __init__(self, source: BinaryIO, lrecl: Optional[int] = None) -> None:
        """
        Initialize the RECFM reader with a source and a logical record length, ``lrecl``.
        :param source: A file opened for binary IO
        :param lrecl: The expected logical record length.
        """
        self.source = source
        self.lrecl = lrecl
        self._used = 0

    @abc.abstractmethod
    def record_iter(self) -> Iterator[bytes]:  # pragma: no cover
        """Yields each physical record, stripped of headers."""
        ...

    def used(self, size: int) -> None:
        """
        Used by a row to announce the number of bytes consumed.
        Supports the rare case of RECFM_N, where records are variable length with no RDW or BDW headers.
        """
        logger.debug("Consumed {0} Bytes".format(size))
        self._used = size


class RECFM_N(RECFM_Reader):
    """
    Read variable-length records without RDW (or BDW).

    In the case of ``Occurs Depending On``, the schema doesn't have  single, fixed size. 
    The client of this class announces how the bytes were actually used.

    ..  code-block:

        with path.open("rb") as source:
            reader = RECFM_N(source)
            for buffer in reader.record_iter():
                # process the buffer, computing the record length, lrecl
                reader.used(lrecl)
    """

    def __init__(self, source: BinaryIO, lrecl: Optional[int] = None) -> None:
        """
        Initialize the RECFM helper.

        :param source: the file
        :param lrecl: in principle this is a maximum, but it's ignored.
        """
        super().__init__(source, 0)
        self.buffer = self.source.read(32768)

    def record_iter(self) -> Iterator[bytes]:
        """
        Provides the entire buffer. The first bytes
        are a record.

        The :py:meth:`used` method informs
        this object how many bytes were used.
        From this, the next record can be returned.

        :yields: blocks of bytes.
        """
        while len(self.buffer) != 0:
            self._used = 0
            yield self.buffer
            if self._used == 0:
                raise RuntimeError(
                    f"No bytes consumed from buffer via the .used() method!"
                )
            self.buffer = self.buffer[self._used :] + self.source.read(
                32768 - self._used
            )


class RECFM_F(RECFM_Reader):
    """
    Read RECFM=F files.

    The schema's record size is the lrecl, logical record length.
    """

    def record_iter(self) -> Iterator[bytes]:
        """
        :yields: physical records, stripped of headers.
        """
        if not self.lrecl:
            raise TypeError(f"{self.__class__.__name__} requires lrecl > 0")
        data = self.source.read(self.lrecl)
        while len(data) != 0:
            yield data
            data = self.source.read(self.lrecl)

    def rdw_iter(self) -> Iterator[bytes]:
        """
        :yields:  records with RDW injected, these look like RECFM_V format as a standard.
        """
        for row in self.record_iter():
            yield struct.pack(">H2x", len(row) + 4) + row


RECFM_FB = RECFM_F


class RECFM_V(RECFM_Reader):
    """
    Read RECFM=V files.

    The schema's record size is irrelevant. 
    Each record has a 4-byte Record Descriptor Word (RDW) followed by the data.
    """

    def record_iter(self) -> Iterator[bytes]:
        """:yields: records, stripped of RDW's."""
        for rdw, row in self._data_iter():
            yield row

    def rdw_iter(self) -> Iterator[bytes]:
        """:yields: records which include the 4-byte RDW."""
        for rdw, row in self._data_iter():
            yield rdw + row

    def _data_iter(self) -> Iterator[tuple[bytes, bytes]]:
        rdw = self.source.read(4)
        while len(rdw) != 0:
            (size,) = struct.unpack(">H2x", rdw)
            data = self.source.read(size - 4)
            yield rdw, data
            rdw = self.source.read(4)


class RECFM_VB(RECFM_Reader):
    """
    Read RECFM=VB files.

    The schema's record size is irrelevant. 
    Each record has a 4-byte Record Descriptor Word (RDW) followed by the data.
    Each block has a 4-byte Block Descriptor Word (BDW) followed by records.
    """

    def record_iter(self) -> Iterator[bytes]:
        """:yields: records, stripped of RDW's."""
        for rdw, row in self._data_iter():
            yield row

    def rdw_iter(self) -> Iterator[bytes]:
        """:yields: records which include the 4-byte RDW."""
        for rdw, row in self._data_iter():
            yield rdw + row

    def bdw_iter(self) -> Iterator[bytes]:
        """:yields: blocks, which include 4-byte BDW and records with 4-byte RDW's."""
        bdw = self.source.read(4)
        while len(bdw) != 0:
            blksize = struct.unpack(">H2x", bdw)[0]
            block_data = self.source.read(blksize - 4)
            yield bdw + block_data
            bdw = self.source.read(4)

    def _data_iter(self) -> Iterator[tuple[bytes, bytes]]:
        bdw = self.source.read(4)
        while len(bdw) != 0:
            blksize = struct.unpack(">H2x", bdw)[0]
            block_data = self.source.read(blksize - 4)
            offset = 0
            while offset != len(block_data):
                assert offset + 4 < len(block_data), "Corrupted Data Block {!r}".format(
                    block_data
                )
                rdw = block_data[offset : offset + 4]
                size = struct.unpack(">H2x", rdw)[0]
                yield rdw, block_data[offset + 4 : offset + size]
                offset += size
            bdw = self.source.read(4)
