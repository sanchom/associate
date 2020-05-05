# Goal

This is a work in progress. I'm trying to improve upon my [first
attempt at an automatic legal citation and layout
system](https://github.com/sanchom/allard-writing-tools). It was too
complicated.

The goal is to let you provide some metadata about your citations,
write using plain text, and then turn that into a nicely formatted PDF
or Word document with proper McGill-style citations, footnotes, and
(optional) bibliography, for both articles and factums.

# Dependencies

* [Racket](https://racket-lang.org/)
* [pollen](https://pkgs.racket-lang.org/package/pollen),
  [pollen-citations-mcgill](https://pkgs.racket-lang.org/package/pollen-citations-mcgill),
  and dependencies
* a LaTeX installation (on Windows, I recommend [MiKTeX](https://miktex.org/))
* [pandoc](https://pandoc.org/)

# Using

1. Install all the above dependencies.
2. Clone this git repository.
3. From the root directory of the cloned repository, run `raco pollen
   render examples/down-the-foxhole.pdf`. This will re-create the
   example pdf.

# Demo

There's an online demo here: https://citations-demo.herokuapp.com/

# Examples

| Document type | Source text   | Output PDF |
| ------------- | ------------- |------------|
| Article       | [examples/down-the-foxhole.poly.pm](examples/down-the-foxhole.poly.pm) | [examples/down-the-foxhole.pdf](examples/down-the-foxhole.pdf) |
| Factum        | work in progress | work in progress |
