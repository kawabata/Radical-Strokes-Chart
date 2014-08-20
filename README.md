Radical Strokes Chart Generator
==============================

This tool creates an HTML-based chart for radical strokes of UCS
Ideographs

## Description

There has already been quite an amount of ideographs encoded in the
UCS. This tool creates a radical-strokes based charts for UCS
Ideographs. If an image of non-UCS ideographs are given to specific
directory, then they are embeeded, too.

This tool finds Radicals and Strokes of individual ideographs from
both UCS standards and its IDS (Ideographic Description Sequence)
information. When there are multiple (ambiguous) strokes, depending on
dictionaries or policy, all these possible storke counts are covered
in this chart. For example, 艹, 之, 㐄, 及, and 辶 are counted in both
3 or 4 strokes.

## Demo

![Sample Screenshot](https://cloud.githubusercontent.com/assets/217020/3961395/6d4b8886-2753-11e4-9c63-b2e74eed48be.png)

## Requirement

- [HanaMinAFDKO Font](http://github.com/cjkvi/HanaMinAFDKO/)
- [Emacs](http://www.gnu.org/software/emacs/) (version 24 or later)
- [Cask](http://cask.github.io/)

## Usage

Run the following commands.

    cask install
    cask exec emacs --script radstr-chart.el

This will creates "radstr.html", that you can print out to produce the
document.

## Licence

[MIT](http://opensource.org/licenses/MIT)

## Author

[kawabata](https://github.com/kawabata)
