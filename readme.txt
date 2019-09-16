EncConv is simplified code of LConvEncoding from Lazarus.
it uses the same codepage tables.

removed:
- using of FreePascal encoding convertion APIs
- using of Unix iconv
- changing of CodePage for resulting string (for newer FreePascal)
changed:
- type of encoding ID is enum
- renamed most of functions, except simple functions like UTF8ToNNN, NNNToUTF8
- added EncConvErrorMode global var, which has 4 possible values
  (LConvEncoding global var had 2 values):
  raise exception, skip chars, replace chars with '?', return empty string
- removed Encoded parameter of functions

author: Alexey Torgashin
license: same as for Lazarus
