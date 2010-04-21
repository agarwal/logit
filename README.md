logit.ml - OCaml script to date stamp files and move them to a log directory  
Project Website: <http://github.com/agarwal/logit>

INSTALLATION
============
logit.ml is provided as a single self-contained OCaml
file. Prerequisites are:

* OCaml
* findlib
* ocamlscript
* calendar
* getopt
* extlib

These are all common OCaml libraries and should be easy to install
(e.g. using [godi](http://godi.camlcity.org)). Assuming these
prerequisites are met, all you have to do is put the single file
logit.ml somewhere in your path. Make sure it is executable (on bash,
do `chmod a+x logit.ml`). Also, you must have write access to the
directory in which you place it (due to how ocamlscript works). Then,
type `logit.ml --help` to test if it is working.


USAGE
=====
Run `logit.ml --help` for documentation.


EXAMPLES
========
Move your notes from today to your log directory:

    logit.ml -d ~/log notes.txt

If today's date is April 20, 2010, this would be equivalent to:

    mv notes.txt ~/log/2010-04-20\ notes.txt

If these notes were from yesterday, but you forgot to run the command
until getting back to work this morning, you can specify a prefix
manually:

    logit.ml -d ~/log -p 2010-04-19 notes.txt

It is recommended to use your shell's facilities to specify a default
log directory. For example, in the bash shell, you can do:

    alias logit='logit.ml -d ~/log'

Then, the first example above becomes simply:

    logit notes.txt

You can override the default by still using the -d option since only
the last takes effect.


LICENSE
=======
This software is distributed under an MIT license as specified in the
header of logit.ml.


CONTACT
=======
[Ashish Agarwal](http://ashishagarwal.org) (<agarwal1975@gmail.com>)  
