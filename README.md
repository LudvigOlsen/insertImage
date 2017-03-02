
<!-- README.md is generated from README.Rmd. Please edit that file -->
insertImage
===========

R package: Insert image from file chooser into Markdown. Copy image to project. Addin for RStudio.

Inserts image code with alternative text specified by user in console.

If image file is not in the project (working directory or subdirectory) the user can copy the image file to the project. An img/ directory is created in the working directory, unless document is in vignettes/ directory in which case the user can choose to copy to this directory instead.

By Ludvig R. Olsen,
Cognitive Science, Aarhus University.
Started in Feb. 2017

Contact at:
<r-pkgs@ludvigolsen.dk>

Main functions:

-   insertImage

Installation
------------

Development version:

> install.packages("devtools")
>
> devtools::install\_github("LudvigOlsen/insertImage")

Use
---

-   Install package.
-   Add key command by going to:
    -   *Tools* &gt; *Addins* &gt; *Browse Addins* &gt; *Keyboard Shortcuts*.
    -   Find **Insert Image** and press its field under *Shortcut*.
    -   Press desired key command.
    -   Press *Apply*.
    -   Press *Execute*.
-   Press chosen key command inside R Markdown document.
