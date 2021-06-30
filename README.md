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
+ `.rktd` file extension for files containing encoded data "as s-expressions".
	- These files do not contain source code. Hence, they must not be executed as source code.
	- These files must not be mandatory/necessary/required, nor imported/loaded. (they should not be executed as code).
	- *Racket* programs can store (encoded) data in these files using `write` and read data from them using `read`.
	- These files are analogous to  .sexp file or a .json file, just pure data.




Reference:
+ Alex Knauth, Answer to "What is the extension for Haskell? [closed]", Stack Exchange Inc., New York, NY, June 10, 2016. Available online from Stack Exchange Inc.: Stack Overflow: Questions at: https://stackoverflow.com/a/37748411/1531728; June 29, 2021 was the last access date.
