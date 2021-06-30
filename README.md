# racket-sandbox

A sandbox to test concepts while learning to program in Racket.

It also include study notes for learning *Racket*.




## Getting Started

### File Extension for *Racket* Programs

File Extensions for *Racket* programs and associated files:
+ `.rkt` file extension for source code files that contain/represent modules.
	- These files typically include the "`#lang` line at the top". It may also include the keyword `module`, which "can be imported as modules \[using the keyword\] `require`."
+ "`.rktl` and `.rkts` file extensions for" source code files "at the top-level".
	- These files are not *Racket* modules, and usually do not have a "`#lang` line at the top."
	- Use `load` to include/load these source code files in an external environment, rather than use the `require` command to import them.
	- They may mutate variables found in imported source code files or modules, and are like scripts of dynamically typed, high-level programming languages; this is discouraged, since variables should not be mutated in functional programming.
The .rktd file extension is used for files that just have data encoded as s-expressions, not code. These files should not be required or loaded (they should not be executed as code). However, other programs use them to store data on the file system using write, and to read the data later using read. It's purpose is the same as a .sexp file or a .json file, just pure data.




Reference:
+ Alex Knauth, Answer to "What is the extension for Haskell? [closed]", Stack Exchange Inc., New York, NY, June 10, 2016. Available online from Stack Exchange Inc.: Stack Overflow: Questions at: https://stackoverflow.com/a/37748411/1531728; June 29, 2021 was the last access date.
