logit.ml - date/time-stamp files and move them to a log directory  
Project Website: <http://github.com/agarwal/logit>

INSTALLATION
============
logit is an OCaml program. Prerequisites are:

* OCaml compiler
* extlib
* getopt
* findlib

These are all standard OCaml libraries and you probably already have
them if you have OCaml. If you don't, installing OCaml is also trivial.

Build the code by running

    make all

Install it by first setting the INSTALL_DIR variable in the Makefile
to the directory you would like to install to. Then run

    make install


USAGE
=====
Run `logit.ml --help` for a quick reference. Examples are given below
and full details follow.


EXAMPLES
========
Date-stamp your notes from today:

    logit.ml notes.txt

If today's date is April 20, 2010, this would be equivalent to:

    mv notes.txt 2010-04-20\ notes.txt

If these notes were from yesterday, but you forgot to run the command
until getting back to work this morning, you can set the day manually:

    logit.ml -o "%y-%m-19 %s" notes.txt

which would be equivalent to:

    mv notes.txt 2010-04-19\ notes.txt

The -o option takes a 'printer' that specifies how the output file
name should be formatted. In this case, we used %y and %m to get the
current year and month. The %s gave the base of the filename. Note
that file extensions are never modified by logit.

If you have a preferred log directory, you can use the -d option:

    logit -d ~/log notes.txt

would be equivalent to:

    mv notes.txt ~/log/2010-04-20\ notes.txt

You can also extract fields from the original file name and use them
to construct the output file name. For example:

    logit -i "%s%y%m%d" -o "%y-%m-%d_%s" notes20100818.txt

would be equivalent to:

    mv notes20100818.txt 2010-08-18_notes.txt

Here, the -i option specifies a scanner that specifies how the input
file name should be parsed. The scanner %s%y%m%d says to parse a
string, then a year, then a month, and finally a day. Then, %y in the
printer refers to the year value extracted from the input file name.

More complex operations are possible by understanding the detailed
reference below.

DETAILED REFERENCE
==================
A scanner is a list of one or more scanner fields:

* a lone character - parses exactly that character
* %s - parses a string up until any subsequent scanner field succeeds
* %y - parses a 4 digit year
* %m - parses a 2 digit month
* %d - parses a 2 digit day
* %h - parses a 2 digit hour
* %n - parses a 2 digit minute
* %c - parses a 2 digit second

The scanner is applied to the input file name after stripping off any
directory and file extension. Each field in the scanner is then mapped
to the particular value parsed in from the file name it was applied
to. An error occurs if the parse is not possible.

Multiple %s scanner fields are allowed but not immediately adjacent to
each other. Only single instances of the date and time fields are
allowed.

A printer is a list of one or more printer fields:

* a lone character - prints exactly that character
* %[n]s - a percent, an optional integer, and then an s. If no integer
   is given, it defaults to 1. It prints the `n`th string extracted by
   the scanner.
* %[(+|-)N]y - a percent, an optional signed integer value, and a
   y. It prints the current year if the scanner did not contain a %y,
   or it prints the year extracted by the scanner if it did. The
   signed integer value is used to first modify this value.
* As above. Instead of y, can use m, d, h, n, or c with analogous
  meaning for month, day, hour, minute, and second.

If not given, the default scanner is "%s" and the default printer is
"%y-%m-%d %s".

LICENSE
=======
This software is distributed under an MIT license as provided in the
LICENSE file distributed with this software.

CONTACT
=======
[Ashish Agarwal](http://ashishagarwal.org) (<agarwal1975@gmail.com>)  

[![Build Status](https://travis-ci.org/agarwal/logit.png?branch=master)](https://travis-ci.org/agarwal/logit)
