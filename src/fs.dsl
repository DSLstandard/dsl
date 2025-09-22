[file
  [import "base.dsl"]

  [tydef FilePath * String]

  // Returns either error message or the UTF-8 text of the file
  [def-decl read_file_utf8 [-> FilePath [HostEither String String]]]
  [def-annotate read_file_utf8 "foreign-import:fs_read_file_utf8"]

  [def-decl pub fs.read_file_utf8 [-> FilePath [Either String String]]]
  [def-impl fs.read_file_utf8 [\ path [
    [read_file_utf8 path . HostEither.to_either]
  ]]]

  // Returns either error message or nothing on success
  [def-decl write_file_utf8 [-> FilePath String [HostEither String ,]]]
  [def-annotate write_file_utf8 "foreign-import:fs_write_file_utf8"]

  [def-decl pub fs.write_file_utf8 [-> FilePath String [Either String ,]]]
  [def-impl fs.write_file_utf8 [\ path content
    [write_file_utf8 path content . HostEither.to_either]
  ]]
]