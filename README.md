# racket-sandbox

A sandbox to test concepts while learning to program in Racket.

It also include study notes for learning *Racket*.




## Getting Started

### File Extension for *Racket* Programs

File Extensions for *Racket* programs and associated files:
+ `.rkt` file extension for source code files that contain/represent modules.
+ These normally have a #lang .... line at the top, or sometimes (module ....). They can be imported as modules with require.

The .rktl and .rkts file extensions are used for files meant to be loaded at the top-level that aren't modules. They don't necessarily have a #lang .... line at the top, and must be loaded in some external environment with load instead of imported with require. These usually have a more "dynamic" feel to them, and they're used more often with scripts that use mutation of variables across multiple files. This is not the "encouraged" style in Racket.

The .rktd file extension is used for files that just have data encoded as s-expressions, not code. These files should not be required or loaded (they should not be executed as code). However, other programs use them to store data on the file system using write, and to read the data later using read. It's purpose is the same as a .sexp file or a .json file, just pure data.




Reference:
+ chepner, Answer to "What is the extension for Haskell? [closed]", Stack Exchange Inc., New York, NY, June 10, 2016. Available online from Stack Exchange Inc.: Stack Overflow: Questions at: https://stackoverflow.com/a/37748411/1531728; June 29, 2021 was the last access date.
