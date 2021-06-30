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
	- These files are analogous to `.sexp` or `.json` files of "pure data."


Reference:
+ Alex Knauth, Answer to "What is the extension for Haskell? [closed]", Stack Exchange Inc., New York, NY, June 10, 2016. Available online from Stack Exchange Inc.: Stack Overflow: Questions at: https://stackoverflow.com/a/37748411/1531728; June 29, 2021 was the last access date.

From *Wikipedia*, the `.rktd` and `.sexp` files seem to contain "S-expressions or symbolic expressions, abbreviated as sexpr or sexp".

Reference:
+ Wikipedia contributors, "S-expression," in *Wikipedia, The Free Encyclopedia: Data serialization formats*, Wikimedia Foundation, San Francisco, CA, June 24, 2021. Available online from *Wikipedia, The Free Encyclopedia: Data serialization formats* at: https://en.wikipedia.org/wiki/S-expression; June 30, 2021 was the last access date.

`.json` files store data in the JSON (JavaScript Object Notation) data exchange format.


Reference:
+ Wikipedia contributors, "JSON," in *Wikipedia, The Free Encyclopedia: Data serialization formats*, Wikimedia Foundation, San Francisco, CA, June 8, 2021. Available online from *Wikipedia, The Free Encyclopedia: Data serialization formats* at: https://en.wikipedia.org/wiki/JSON; June 30, 2021 was the last access date.




## Adding Comments to Source Code




# References

+ Jesse Alama, Claire Alvis, Leif Andersen, Yavuz Arkun, Michael Ballantyne, Ian Barland, Eli Barzilay, Gann Bierner, Stephen Bloch, Matthew Butterick, Filipe Cabecinhas, Stephen Chang, Richard Cleis, John Clements, Richard Cobbe, Greg Cooper, Ryan Culpepper, Stephen De Gabrielle, Christos Dimoulas, Eric Dobson, Carl Eastlund, Moy Easwaran, Will Farr, Matthias Felleisen, Dan Feltey, Burke Fetscher, Michael Filonenko, Robby Findler, Jack Firth, Kathi Fisler, Cormac Flanagan, Matthew Flatt, Spencer Florence, Fred Fu, Tony Garnock-Jones, Dionna Amalie Glaze, Sebastian Good, Paul Graunke, Kathy Gray, Ben Greenman, Dan Grossman, Arjun Guha, Dave Gurnell, Tobias Hammer, Alex Harsányi, William Hatch, Bruce Hauman, Greg Hendershott, Dave Herman, Blake Johnson, Andrew Kent, Alexis King, Casey Klein, Alex Knauth, Geoffrey S. Knauth, Mark Krentel, Shriram Krishnamurthi, Mario Latendresse, Xiangqi Li, Guillaume Marceau, Gustavo Massaccesi, Paulo Matos, Jacob Matthews, Jay McCarthy, Mike T. McHenry, Philippe Meunier, Laurent Orseau, Scott Owens, Pavel Panchekha, David T. Pierson, Bogdan Popa, Sorawee Porncharoenwase, Jon Rafkind, Jamie Raymond, Grant Rettke, Paul Schlie, Dorai Sitaram, Francisco Solsona, Sarah Spall, Mike Sperber, Vincent St-Amour, Paul Steckler, Stevie Strickland, James Swaine, Jens Axel Søgaard, Asumu Takikawa, Kevin Tew, Sam Tobin-Hochstadt, Neil Toronto, Milo Turner, Dale Vaillancourt, Neil Van Dyke, David Van Horn, Anton van Straaten, Dimitris Vyzovitis, Stephanie Weirich, Noel Welsh, Adam Wick, Danny Yoo, Shu-Hung You, yjqww6, Jon Zeppieri, and ChongKai Zhu, "Racket Documentation," from Racket: Docs, Version 8.1, May, 2021. Available from Racket: Docs at: https://docs.racket-lang.org/; June 30, 2021 was the last access date.
+ Matthew Flatt, Robert Bruce Findler, and PLT Inc. staff, "The Racket Guide," from Racket: Docs: Racket Documentation: Racket Language and Core Libraries, Version 8.1, May, 2021. Available from Racket: Docs: Racket Documentation: Racket Language and Core Libraries at: https://docs.racket-lang.org/guide/index.html; June 30, 2021 was the last access date.
+ Matthew Flatt, Robert Bruce Findler, and PLT Inc. staff, "The Racket Reference," from Racket: Docs: Racket Documentation: Racket Language and Core Libraries, Version 8.1, May, 2021. Available from Racket: Docs: Racket Documentation: Racket Language and Core Libraries at: https://docs.racket-lang.org/reference/index.html; June 30, 2021 was the last access date.





