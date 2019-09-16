EncConv is simplified code of LConvEncoding from Lazarus.
it uses the same codepage tables.

changes:
- type of encoding ID is now enum
- removed using of FPC string convertion API, removed using of Unix iconv;
  so unit must work the same way on all platforms and FPC versions
- removed optional changing of CodePage for resulting string
- renamed most of functions, except simple functions like UTF8ToNNN, NNNToUTF8
- added EncConvErrorMode global var, which has 4 possible values
  (LConvEncoding global var had 2 values):
  raise exception, skip chars, replace chars with '?', return empty string
- removed parameter Encoded of functions

author: Alexey Torgashin
license: same as for Lazarus
