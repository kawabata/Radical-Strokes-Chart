Radical Strokes Chart Generator
==============================

This tool creates an HTML-based chart for radical strokes of UCS
Ideographs

## Description

There has already been quite an amount of ideographs encoded in the
UCS. This tool creates a radical-strokes based charts (in HTML) for
*all* UCS Ideographs. If an image of non-UCS ideographs are given to
specific directory, then they will also be embedded, too.

This tool finds radicals and strokes of individual ideographs from
both UCS standards and its IDS (Ideographic Description Sequence)
information. If there are multiple (ambiguous) possible strokes,
depending on dictionaries or policy, then all such possible storke
counts would covered in this chart. For example, 艹, 之, 㐄, 及, and
辶 are counted in both 3 or 4 strokes.

All ideographs that contains "月" components on topmost IDS structure
are collected to "肉" radical besides its original radical-strokes
position.

## Sample Screenshot

![Sample Screenshot](https://cloud.githubusercontent.com/assets/217020/3961395/6d4b8886-2753-11e4-9c63-b2e74eed48be.png)

## Requirements

- [HanaMinAFDKO Font](http://github.com/cjkvi/HanaMinAFDKO/)
- [Emacs](http://www.gnu.org/software/emacs/) (version 24 or later)
- [Cask](http://cask.github.io/)

## Usage

PDF files can be downloaded from
[releases](https://github.com/kawabata/Radical-Strokes-Chart/releases).
If you want to create the file manually, then run the following
commands.

    cask install
    cask exec emacs --script radstr-chart.el

This will create "radstr.html", that you can print out to produce the
document.

## Licence

[GPL](http://www.gnu.org/licenses/)

## Author

[kawabata](https://github.com/kawabata)
