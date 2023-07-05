## Executable

The `hasherize` executable creates a copy of a directory in which each
direct child of the directory has a hasherized name.

For example, suppose the input directory looks like this:

```
├── favicon.ico
└── fonts
    ├── css
    │   └── fonts.css
    └── fonts
        ├── FiraMono-Bold.woff2
        └── FiraMono-Regular.woff2
```

The output directory will look something like this:

```
├── d7555833df923044e820a82a040850268d80ca2a8b4ade4a7ecb1aaa709f503a-favicon.ico
└── b7114fb092b385a4419e4aafa30075ded6fe3232219dcb75f49afbc24bbee39c-fonts
    ├── css
    │   └── fonts.css
    └── fonts
        ├── FiraMono-Bold.woff2
        └── FiraMono-Regular.woff2
```

Additionally, a CSV file will be generated that maps input names to hashed names:

```
favicon.ico,d7555833df923044e820a82a040850268d80ca2a8b4ade4a7ecb1aaa709f503a-favicon.ico
fonts,b7114fb092b385a4419e4aafa30075ded6fe3232219dcb75f49afbc24bbee39c-fonts
```

### Parameters

The input and output paths must be specified by environment variables:

* `input-directory` - Where to read input from
* `output-directory` - Where to write hashed files to
* `output-manifest` - Where to put the CSV file


## Library

This library provides a function called `getHash` which computes a hash
of a file or directory.

```haskell
Hasherize.getHash :: (MonadIO m) => OsPath -> m ByteString
```


## Hashing system

A *path segment* is Unicode text.
The hash of a path segment is the SHA-256 digest of its UTF-8 encoding.

A *path* is a list of path segments.
The hash of a path is determined by hashing each segment, concatenating,
and taking a SHA-256 digest.

An *entry* consists of a path and the content of a file.
The hash of an entry is determined by hashing the path, hashing the
content, concatenating, and taking a SHA-256 digest.

The *enumeration* of a path is a list of entries.

* If the path is a file *x*, its enumeration consists of a single
  entry, *([], x)*.
* If the path is a directory, its enumeration consists of an entry for
  each file recursively contained within the directory, sorted by path.
  We assume that the file system contains no directory cycles.

The result returned by the `getHash` function is produced by enumerating
the path, hashing each entry, concatenating, and taking a SHA-256 digest.

The `hasherize` executable produces a hashed file name by concatenating
the hash digest in hexadecimal form, the hyphen character `-`, and the
original file name.
