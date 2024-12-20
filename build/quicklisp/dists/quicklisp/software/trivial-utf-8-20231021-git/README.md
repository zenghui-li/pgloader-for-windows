<a id="x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# Trivial UTF-8 Manual

## Table of Contents

- [1 The trivial-utf-8 ASDF System][d9f2]
- [2 Introduction][e14a]
- [3 Links][3c3e]
- [4 Reference][a043]

###### \[in package TRIVIAL-UTF-8\]
<a id="x-28-22trivial-utf-8-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

## 1 The trivial-utf-8 ASDF System

- Description: A small library for doing UTF-8-based input and output.
- Licence: ZLIB
- Author: Marijn Haverbeke <marijnh@gmail.com>
- Maintainer: Gábor Melis <mega@retes.hu>
- Homepage: [https://common-lisp.net/project/trivial-utf-8/](https://common-lisp.net/project/trivial-utf-8/)
- Bug tracker: [https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8/-/issues](https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8/-/issues)
- Source control: [GIT](https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8.git)

<a id="x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 2 Introduction

Trivial UTF-8 is a small library for doing UTF-8-based in- and
output on a Lisp implementation that already supports Unicode -
meaning [`CHAR-CODE`][4720] and [`CODE-CHAR`][772c] deal with Unicode character codes.

The rationale for the existence of this library is that while
Unicode-enabled implementations usually do provide some kind of
interface to dealing with character encodings, these are typically
not terribly flexible or uniform.

The Babel library solves a similar problem while
understanding more encodings. Trivial UTF-8 was written before Babel
existed, but for new projects you might be better off going with
Babel. The one plus that Trivial UTF-8 has is that it doesn't depend
on any other libraries.

[babel]: https://common-lisp.net/project/babel/


<a id="x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-LINKS-20MGL-PAX-3ASECTION-29"></a>

## 3 Links

Here is the [official repository][trivial-utf-8-repo] and the
[HTML documentation][trivial-utf-8-doc] for the latest version.

[trivial-utf-8-repo]: https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8

[trivial-utf-8-doc]: http://melisgl.github.io/mgl-pax-world/trivial-utf-8-manual.html


<a id="x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-REFERENCE-20MGL-PAX-3ASECTION-29"></a>

## 4 Reference

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-BYTE-LENGTH-20FUNCTION-29"></a>

- [function] **UTF-8-BYTE-LENGTH** *STRING*

    Calculate the amount of bytes needed to encode `STRING`.

<a id="x-28TRIVIAL-UTF-8-3ASTRING-TO-UTF-8-BYTES-20FUNCTION-29"></a>

- [function] **STRING-TO-UTF-8-BYTES** *STRING &KEY NULL-TERMINATE*

    Convert `STRING` into an array of unsigned bytes containing its UTF-8
    representation. If `NULL-TERMINATE`, add an extra 0 byte at the end.

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-GROUP-SIZE-20FUNCTION-29"></a>

- [function] **UTF-8-GROUP-SIZE** *BYTE*

    Determine the amount of bytes that are part of the character whose
    encoding starts with `BYTE`. May signal [`UTF-8-DECODING-ERROR`][ad1e].

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-BYTES-TO-STRING-20FUNCTION-29"></a>

- [function] **UTF-8-BYTES-TO-STRING** *BYTES &KEY (START 0) (END (LENGTH BYTES))*

    Convert the `START`, `END` subsequence of the array of `BYTES` containing
    UTF-8 encoded characters to a [`STRING`][b93c]. The element type of
    `BYTES` may be anything as long as it can be [`COERCE`][6d29]d into
    an `(UNSIGNED-BYTES 8)` array. May signal [`UTF-8-DECODING-ERROR`][ad1e].

<a id="x-28TRIVIAL-UTF-8-3AREAD-UTF-8-STRING-20FUNCTION-29"></a>

- [function] **READ-UTF-8-STRING** *INPUT &KEY NULL-TERMINATED STOP-AT-EOF (CHAR-LENGTH -1) (BYTE-LENGTH -1)*

    Read UTF-8 encoded data from `INPUT`, a byte stream, and construct a
    string with the characters found. When `NULL-TERMINATED` is given,
    stop reading at a null character. If `STOP-AT-EOF`, then stop at
    [`END-OF-FILE`][fe09] without raising an error. The `CHAR-LENGTH` and
    `BYTE-LENGTH` parameters can be used to specify the max amount of
    characters or bytes to read, where -1 means no limit. May signal
    [`UTF-8-DECODING-ERROR`][ad1e].

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-DECODING-ERROR-20CONDITION-29"></a>

- [condition] **UTF-8-DECODING-ERROR** *[SIMPLE-ERROR][cac1]*

  [3c3e]: #x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [4720]: http://www.lispworks.com/documentation/HyperSpec/Body/f_char_c.htm "CHAR-CODE (MGL-PAX:CLHS FUNCTION)"
  [6d29]: http://www.lispworks.com/documentation/HyperSpec/Body/f_coerce.htm "COERCE (MGL-PAX:CLHS FUNCTION)"
  [772c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_code_c.htm "CODE-CHAR (MGL-PAX:CLHS FUNCTION)"
  [a043]: #x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-REFERENCE-20MGL-PAX-3ASECTION-29 "Reference"
  [ad1e]: #x-28TRIVIAL-UTF-8-3AUTF-8-DECODING-ERROR-20CONDITION-29 "TRIVIAL-UTF-8:UTF-8-DECODING-ERROR CONDITION"
  [b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"
  [cac1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_er.htm "SIMPLE-ERROR (MGL-PAX:CLHS CONDITION)"
  [d9f2]: #x-28-22trivial-utf-8-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"trivial-utf-8" ASDF/SYSTEM:SYSTEM'
  [e14a]: #x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [fe09]: http://www.lispworks.com/documentation/HyperSpec/Body/e_end_of.htm "END-OF-FILE (MGL-PAX:CLHS CONDITION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
