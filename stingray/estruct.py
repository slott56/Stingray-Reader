""" 
The :py:mod:`estruct` module unpacks EBCDIC-encoded values. It is a big-endian version of the :py:mod:`struct` module. 
It uses two COBOL clauses, ``USAGE`` and ``PIC``, to describe the format of data represented by a sequence of bytes. 

The format string can be ``'USAGE DISPLAY PIC S999.99'``, for example. A full DDE line of code is tolerated, making it easier to transform COBOL to :py:mod:`estruct` formats.

The :py:mod:`struct` module uses a compact format string describe data. 
This string is used unpack text, int, and float values from a sequence of bytes. See https://docs.python.org/3/library/struct.html#format-characters. 
 
An alternative interface for this module could be to use single-letter codes. ``15x`` for display. ``f`` and ``d`` for COMP-1 and COMP-2. ``9.2p`` for ``PIC 9(9)V99`` packed decimal COMP-3. ``9.2n`` for zoned decimal text. Plus ``h``, ``i``, and ``l`` for COMP-4 variants. This seems needless, but it is compact and somewhat more compatible with the :py:mod:`struct` module.

File Reading
============

The EBCDIC files can include physical "Record Format" (RECFM) assistance.
These classes define a number of Z/OS RECFM conversion. We recognize four
actual RECFM's plus an additional special case.

-   F - Fixed.

-   FB - Fixed Blocked.

-   V - Variable, each record is preceded by a 4-byte Record Description Word (RDW).

-   VB - Variable Blocked. Blocks have Block Description Word (BDW); each record within a block has a Record Description Word.

-   N - Variable, but without BDW or RDW words. This involves some buffer management
    magic to recover the records properly. This is required to handle ``Occurs Depending On`` cases
    where there's no V or VB header. This requires the consumer of bytes to announce how many bytes
    were consumed so the reader can advance an appropriate amount.

..  note::  IBM z/Architecture mainframes are all big-endian

Module Contents
===============

-  :py:class:`Representation` extracts details of representation from COBOL DDE.

-  :py:func:`unpack` Unpacks EBCDIC data.

-  :py:class:`calcsize` Computes the size of an atomic field.

-  :py:class:`RECFM_Reader` a family of classes to read data in various physical formats.
"""

import abc
import logging
from typing import Any, NamedTuple, Type, Optional, Dict, TextIO, BinaryIO, Iterator
import re
from decimal import Decimal
import struct

logger = logging.getLogger("stingray.estruct")

class DesignError(BaseException):
    pass

clause_pattern = re.compile(
    r"(?:USAGE\s+)?(?:IS\s+)?(?P<usage>BINARY|COMPUTATIONAL-1|COMPUTATIONAL-2|COMPUTATIONAL-3|COMPUTATIONAL-4|COMPUTATIONAL|COMP-1|COMP-2|COMP-3|COMP-4|COMP|DISPLAY|PACKED-DECIMAL)"
    r"|(?:PIC|PICTURE)\s+(?:IS\s+)?(?P<picture>\S+)"
)


class Representation(NamedTuple):
    """COBOL Representation Details: Usage and Picture.
    
    >>> Representation.parse("USAGE DISPLAY PICTURE S9(5)V99")
    Representation(usage='DISPLAY', picture_elements=[{'sign': 'S'}, {'digit': '99999'}, {'decimal': 'V'}, {'digit': '99'}], picture_size=8, pattern='[ +-]?\\\\d\\\\d\\\\d\\\\d\\\\d\\\\d\\\\d')
    """

    usage: str
    picture_elements: list[dict[str, str]]
    picture_size: int
    pattern: str

    @staticmethod
    def normalize_picture(source: str) -> list[dict[str, str]]:
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
                # Normalize Picture: expand 9(5) to 99999.
                # Mypy doesn't like the assignment below.
                # It's not clear the value of pic.groupdict()["repeat"] will be
                # large enough to support 4 or more substrings.
                count: str
                char: str
                char, left, *count, right = pic.groupdict()["repeat"]  # type: ignore [misc]
                picture.append({"digit": int("".join(count)) * char})
            else:
                non_empty = {n: v for n, v in pic.groupdict().items() if v}
                picture.append(non_empty)
        if ending != len(source):
            raise ValueError(
                f"Invalid characters {source[ending:]!r} in PIC {source!r}"
            )
        return picture

    @classmethod
    def parse(cls: Type["Representation"], format: str) -> "Representation":
        """Parse the COBOL DDE information."""
        usage = "DISPLAY"
        pic_source = ""
        picture: list[dict[str, str]] = []
        size = 0

        # Find USAGE and PICTURE clauses
        clause_iter = clause_pattern.finditer(format)
        for c in clause_iter:
            if c.groupdict()["usage"]:
                usage = c.groupdict()["usage"]
            elif c.groupdict()["picture"]:
                pic_source = c.groupdict()["picture"]
                picture = cls.normalize_picture(pic_source)

        # Summarize Picture Clause
        # TODO: This is a little too flexible: sign is allowed to repeat, for example.
        regexp: list[str] = []
        for elt in picture:
            if sign := elt.get("sign", ""):
                size += len(sign)
                regexp += sign.replace("S", "[ +-]?")
            elif char := elt.get("char", ""):
                size += len(char)
                regexp += (
                    char.replace("$", "\\$").replace("*", "\\*").replace("B", "\\s")
                )
            elif decimal := elt.get("decimal", ""):
                size += 1 if decimal == "." else 0
                regexp += "\\." if decimal == "." else ""
            elif digits := elt.get("digit", ""):
                # A, X, Z, 9, 0 count.
                # P doesn't count.
                size += len(digits)
                regexp += (
                    digits.replace("A", "\\w")
                    .replace("X", ".")
                    .replace("Z", "\\d")
                    .replace("9", "\\d")
                    .replace("0", "\\d")
                    .replace("P", "")
                )
            elif repeat := elt.get("repeat"):  # pragma: no cover
                # The PICTURE clause was not normalized.
                raise DesignError(f"failure to normalize picture {picture!r}")
            else:  # pragma: no cover
                # The regex pattern and the if statement don't match.
                raise DesignError(f"unexpected {elt} in picture {picture!r}")

        return cls(usage, picture, size, "".join(regexp))


def unpack(format: str, buffer: bytes) -> Any:
    """
    Unpack EBCDIC bytes given a COBOL DDE format specification and a buffer of bytes.
    
    USAGE DISPLAY special case: "external decimal" sometimes called "zoned decimal".
    The PICTURE character-string of an external decimal item can contain only:
    One or more of the symbol 9
    The operational-sign, S
    The assumed decimal point, V
    One or more of the symbol P
        
    External decimal items with USAGE DISPLAY are sometimes referred to as zoned decimal items. 
    Each digit of a number is represented by a single byte. 
    The 4 high-order bits of each byte are zone bits; 
    the 4 high-order bits of the low-order byte represent the sign of the item. 
    The 4 low-order bits of each byte contain the value of the digit.

    TODO: Add support for COMP-1 and COMP-2.
    """
    usage, picture, size, pattern = Representation.parse(format)

    # Decompose PIC into 4 groups: sign, digits, decimal, digits.
    # The group switches from 1 to 3 when the decimal is seen.
    # TODO: Refactor into Representation class
    group = 1  # pre-decimal
    digit_groups = ["", "", "", ""]
    for d in picture:
        if decimal := d.get("decimal"):
            digit_groups[2] = decimal
            group = 3  # post-decimal
        elif digit_chars := d.get("digit"):
            digit_groups[group] += digit_chars
        elif sign_char := d.get("sign"):
            digit_groups[0] = sign_char

    # Usage Display can be text or zoned decimal
    if usage in ("DISPLAY",):
        # Depends on PIC. If PIC has only S9VP, then it's "ZONED DECIMAL"
        edit_options: list[str] = list(filter(None, (x.get("char") for x in picture)))
        numeric_options = (
            size != 0,  # If PIC omitted, size will be zero, and it's a GROUP element.
            digit_groups[0] in {"", "S", None},
            all(d == "9" for d in digit_groups[1]),
            digit_groups[2] in {"V", "", None},
            all(d == "9" for d in digit_groups[3]),
        )
        zoned_decimal = all(numeric_options) and not edit_options
        logger.debug(f"PIC {digit_groups}: all({numeric_options}) and not {edit_options} = {zoned_decimal}")
        if zoned_decimal:
            text = "".join(str(b & 0x0F) for b in buffer)
            sign_half = (buffer[-1] & 0xF0) >> 4
            sign = -1 if (sign_half == 0x0B or sign_half == 0x0D) else +1
            base = Decimal(text)
            scale = Decimal(10) ** (-len(digit_groups[3]))
            return base * scale * sign
        # Match text with a regular expression derived from the picture to see if it's valid.
        text = buffer.decode("CP037")
        logger.debug(f'estruct.unpack: {buffer!r} == {text=}')
        if not re.match(pattern, text):
            raise ValueError(f"{text!r} doesn't match pattern {pattern!r}")
        # It's display text.
        return text

    # Usage COMP-3
    elif usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL",):
        # unpack buffer bytes into pairs of digits
        half_bytes = []
        for b in buffer:
            half_bytes.append((b & 0xF0) >> 4)
            half_bytes.append((b & 0x0F))
        *digits, sign_half = half_bytes
        # get sign and base numeric value
        sign = -1 if (sign_half == 0x0B or sign_half == 0x0D) else +1
        base = Decimal("".join(str(d) for d in digits))
        scale = Decimal(10) ** (-len(digit_groups[3]))
        return base * scale * sign

    # elif usage in ("COMPUTATIONAL-1", "COMP-1")
    #   Unpack 4 byte float
    #   f, = struct.unpack(">f", buffer)
    #   return f

    # elif usage in ("COMPUTATIONAL-2", "COMP-2")
    #   Unpack 8 byte float
    #   f, = struct.unpack(">d", buffer)
    #   return f

    elif usage in ("COMP-4", "COMPUTATIONAL-4", "BINARY", "COMP", "COMPUTATIONAL",):
        # Unpack 2-, 4-, or 8-byte int. Size depends on the length of digit_groups[1]
        # 1-4 digits is two bytes. 5-9 digits is 4 bytes. 10-18 is 8 bytes.
        if len(digit_groups[1]) < 5:
            format = ">h"
        elif 5 <= len(digit_groups[1]) < 10:
            format = ">i"
        elif 10 <= len(digit_groups[1]) < 18:
            format = ">q"
        (n,) = struct.unpack(format, buffer)
        return n

    else:  # pragma: no cover
        raise RuntimeError(f"Usage {usage!r} not supported")


def calcsize(format: str) -> int:
    """
    Compute the size, in bytes for an elementary (non-group-level) COBOL DDE format specification.
    """
    usage, picture, size, pattern = Representation.parse(format)
    if size == 0:
        raise ValueError(f"Problem parsing {format!r}")

    if usage in ("DISPLAY",):
        return size
    elif usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL",):
        return (size + 1) // 2
    elif usage in ("COMP-1", "COMPUTATIONAL-1",):
        return 4
    elif usage in ("COMP-2", "COMPUTATIONAL-2",):
        return 8
    elif usage in ("COMP-4", "COMPUTATIONAL-4", "BINARY", "COMP", "COMPUTATIONAL",):
        return 2 if size < 5 else (4 if 5 <= size < 10 else 8)
    else:
        # This is a design error: regular expression doesn't match this if statement
        raise RuntimeError(f"Unparsable {usage} in picture: {format!r}")  # pragma: no cover


class RECFM_Reader(abc.ABC):
    """Read records based on a physical file format."""

    def __init__(self, source: BinaryIO, lrecl: Optional[int] = None) -> None:
        self.source = source
        self.lrecl = lrecl
        self._used = 0

    @abc.abstractmethod
    def record_iter(self) -> Iterator[bytes]:  # pragma: no cover
        """Returns each physical record, stripped of headers."""
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
    """

    def __init__(self, source: BinaryIO, lrecl: Optional[int] = None) -> None:
        """
        :param source: the file
        :param lrecl: a maximum, but it's ignored.
        """
        super().__init__(source, 0)
        self.buffer = self.source.read(32768)

    def record_iter(self) -> Iterator[bytes]:
        while len(self.buffer) != 0:
            self._used = 0
            yield self.buffer
            if self._used == 0:
                raise RuntimeError(
                    f"No bytes consumed from buffer via the .used() method!"
                )
            self.buffer = self.buffer[self._used:] + self.source.read(
                32768 - self._used
            )


class RECFM_F(RECFM_Reader):
    """
    Read RECFM=F. 
    The schema's record size is the lrecl, logical record length.
    """

    def record_iter(self) -> Iterator[bytes]:
        if not self.lrecl:
            raise TypeError(f"{self.__class__.__name__} requires lrecl > 0")
        data = self.source.read(self.lrecl)
        while len(data) != 0:
            yield data
            data = self.source.read(self.lrecl)

    def rdw_iter(self) -> Iterator[bytes]:
        """Yield rows with RDW injected, these look like RECFM_V format as a standard."""
        for row in self.record_iter():
            yield struct.pack(">H2x", len(row) + 4) + row


RECFM_FB = RECFM_F


class RECFM_V(RECFM_Reader):
    """
    Read RECFM=V.
    The schema's record size is irrelevant. 
    Each record has a 4-byte Record Descriptor Word (RDW) followed by the data.
    """

    def record_iter(self) -> Iterator[bytes]:
        """Iterate over records, stripped of RDW's."""
        for rdw, row in self._data_iter():
            yield row

    def rdw_iter(self) -> Iterator[bytes]:
        """Iterate over records which include the 4-byte RDW."""
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
    Read RECFM=VB. 
    The schema's record size is irrelevant. 
    Each record has a 4-byte Record Descriptor Word (RDW) followed by the data.
    Each block has a 4-byte Block Descriptor Word (BDW) followed by records.
    """

    def record_iter(self) -> Iterator[bytes]:
        """Iterate over records, stripped of RDW's."""
        for rdw, row in self._data_iter():
            yield row

    def rdw_iter(self) -> Iterator[bytes]:
        """Iterate over records which include the 4-byte RDW."""
        for rdw, row in self._data_iter():
            yield rdw + row

    def bdw_iter(self) -> Iterator[bytes]:
        """Iterate over blocks, which include 4-byte BDW and records with 4-byte RDW's."""
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
