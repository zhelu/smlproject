smlproject
==========

MS Project

My master's project, looking at features of expert code written in ML.

To load project files, at the interactive prompt in SML/NJ, type:

 prompt> CM.make "allfiles.cm";

This should load all of the structures in this project and the SML/NJ
visible compiler.

Much of the work in parsing is accomplished by the ParseFile structure.
There are some nice utility functions for extracting information
from the ML parse tree.

The ManyFiles structure allows one to perform aggregate analyses on
a list of files represented in input as a list of strings (filenames).
You can load a list of filenames using ManyFiles.getFileList function.

The Violations structure contains functions to find all of the style
violations recognized by this project.

The AstType structure represents the values from Ast.dec and Ast.exp
ADTs since we can't easily create values if we want to pattern match
or search for a specific type of expression or declaration.

The Counter structure presents a simple interface for counting anything.
