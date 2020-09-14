Latin Engine
============

A library and command line applictions to assist with the translation of Latin
texts. There are packages to this project:

1. `latin-engine-core`: This package implements the core data structures for
working with the source texts: a parse-tree forest, parsing paragraphs into
sentences and parsing sentences into words.

2. `latin-engine-interactive`: This package implements a command line
application for analysing texts. It has a UI for building forests of parse
trees on a per-sentence basis. A forest of parse trees is a set of parse trees
that reflect the grammatical hierachical structure of a sentence. Additionally,
there is another window for making arbitrary annotations to any word in the
sentence. This is useful, for example, to annotate a noun's syntactic case,
gender and number and its grammatical function.

3. `latin-engine-diagrams`: Uses the `diagrams` package to render a parse-tree
forest to pdf, using colourful arcs to show the sentence structure.

4. `latin-engine-json`: JSON serialisation. This provides a Haskell-independent
serialisation format for parse-tree forests and annotations.


Build Instructions
------------------

Use `cabal build`.

Using the `interactive` Mode
----------------------------

To begin, you'll need a piece of latin source text, e.g., from
https://thelatinlibrary.com/. Store a piece of plain text you want to
translate in a .txt file. It's recommended to limit the piece of text in each
file to a relatively small unit, e.g. a single paragraph. A current limitation
of the application is that it recognises sentences based on a period followed
by whitespace. This can lead to problems when the text contains initials.
You must manually delete the whitespace following a period that is not a full
stop (e.g., in the initials of a person).

You can load the file by passing it as a command line argument, e.g.,

```
$ interactive latin/DBG/Caesar_DBG_1.txt
```

You should then see the text displayed in the upper window, with the first
sentence having focus. Below each word is the word's identifier, a number.
Next to this number the application renders the word's status (position in
the parse tree), which is either empty, `R` for a parse tree root or `C`
followed by a number indicating that the word is the child of some other word.

You can change the status of a word with the commands listed at the bottom:
press `r` to make a word into a parse tree root, `c` to make the word a child
of some other word, and 'e' to clear the status. Note, `e` will not work if
the word has children.

You can add annotations by pressing the `a` key, and remove them with the `u`
key. Press `f` to switch the focus between the upper and lower window.

You can use the up or down arrow keys to move between sentences and
annotations, depending on which window has focus.

You can press `s` to save the current state of the program. The parse-tree
forests is saved in an `.fst.json` file, and the annotations are saved in an
`.ann.json` file. Saved forests and annotations are loaded automatically when
you open the program.

You can quit the application using `q` or by pressing the escape key.


Using the `render` Application
------------------------------

You can render a representation of the parse-tree forest to PDF using the
`render` command, e.g.,

```
$ render latin/DBG/Caesar_DBG_1.txt
```

This looks for an associated `.fst.json` file and then renders the text and its
parse-tree forest using colourful arcs and boxes. The result is saved
in a PDF bearing the same name as the input file.
For practical translation, it can be useful to have both the interactive
command and the pdf open. You might even use a file-watch command to
automatically re-render the pdf.

The application uses a variant of the Knuth-Plass algorithm, modified to avoid
breaking a subclause over multiple lines. This is the adaptive algorithm
mentioned below. It exposes two knobs: `COLUMNS`, which is the desired number
of columns per line, and `TOLERANCE`. The algorithm tries to avoid creating
lines whose lenght differst from `COLUMNS` by more than `TOLERANCE`.
Default values are 80 and 20, respectively. See the documentation in
`Diagrams.LatinEngine.LineBreaking`, for more details.

The paragraphs above describes the default behaviour, which can be tweaked by
passing a command line argument.
The full list of command line arguments is as follows:

```
Usage: render [-o|--output FILE] [-f|--forest FORESTFILE] 
              [(-d|--day-mode) | (-n|--night-mode)] 
              [-p|--paragraph-skip PARAGRAPHSKIP] [-l|--line-skip LINESKIP] 
              [-w|--word-skip WORDSKIP] [-s|--scale SCALE] 
              [(-a|--adaptive) | (-r|--word-wrap)] [-c|--columns COLUMNS] 
              [-t|--tolerance TOLERANCE] TARGET
  Render the TARGET text's parse forests to PDF.

Available options:
  -o,--output FILE         File to write output to
  -f,--forest FORESTFILE   File to read the forests from (in json format)
  -d,--day-mode            Day mode, light background, dark text
  -n,--night-mode          Night mode, dark background, light text
  -p,--paragraph-skip PARAGRAPHSKIP
                           Space between paragraphs
  -l,--line-skip LINESKIP  Space between lines
  -w,--word-skip WORDSKIP  Space between words
  -s,--scale SCALE         Scaling factor of the final output
  -a,--adaptive            Use adaptive line breaking to avoid breaking
                           subclauses
  -r,--word-wrap           Use simple line breaking that wraps lines at a
                           certain column width
  -c,--columns COLUMNS     Desired number of columns per line
  -t,--tolerance TOLERANCE Tolerance for the adaptive line breaking. The
                           algorithm tries to avoid creating lines whose length
                           differs from COLUMNS by more than TOLERANCE.
  TARGET                   File containing the text to be rendered
  -h,--help                Show this help text
```