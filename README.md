Radical Strokes Chart Generator
==============================

This tool creates a chart for radical strokes.

## Description

There has been tremondous amount of ideographs encoded in UCS. This
tool creates radical-strokes based charts for UCS. If image of non-UCS
ideographs are given to specific directory, then they are embeeded,
too.

## Demo

![Sample Screenshot](https://cloud.githubusercontent.com/assets/217020/3961395/6d4b8886-2753-11e4-9c63-b2e74eed48be.png)

## Requirement

- [Emacs](http://www.gnu.org/software/emacs/) (version 24 or later)
- [Cask](http://cask.github.io/)

## Usage

Run the following commands.

    cask install
    cask run emacs --script radstr-chart.el

This will creates "radstr.html", that you can print out to produce the
document.

## Licence

[MIT](http://opensource.org/licenses/MIT)

## Author

[kawabata](https://github.com/kawabata)
