#' @title Insert image.
#' @description Addin for RStudio for inserting image into R Markdown.
#'
#' See \code{details} for setting up key command.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @return Inserts \code{![alt](path/to/file)}
#' @details How to set up key command in RStudio:
#'
#' After installing package.
#' Go to:
#'
#' \code{Tools} >> \code{Addins} >> \code{Browse Addins} >> \code{Keyboard Shortcuts}.
#'
#' Find \code{"Insert Image"} and press its field under \code{Shortcut}.
#'
#' Press desired key command.
#'
#' Press \code{Apply}.
#'
#' Press \code{Execute}.
insertImage <- function(){

  #
  # Let's user insert image with file chooser
  # Allows copying image to project in /img/ OR
  # allows copying image to /vignettes/ if working in that
  # Let's user add alternative text via console
  #

  # Get document context
  # to get cursor position
  adc <- rstudioapi::getActiveDocumentContext()

  # Get cursor position
  start <- adc$selection[1][[1]]$range$start

  # Get current working directory
  workDir <- getwd()

  # Get directory path of document
  document_path <- dirname(adc$path)

  # Remove working directory from document_path
  # Then we know which subfolders of the wd the document is in
  document_sub_dirs <- substr(document_path, nchar(workDir)+1,
                              nchar(document_path))

  # Choose image
  # opens browser
  img_source <- file.choose()

  # Get filename
  base <- basename(img_source)

  # Check if file is png or jpg
  # Else throw warning and continue
  if (!substr(base, nchar(base)-2,
              nchar(base)) %in%
      c("png","PNG", "jpg", "JPG")){

    warning("File doesn\'t seem to be png or jpg image")

  }

  # Ask user whether or not to copy file to project
  moveOrNot <- readline("Copy file to project? (y/n): ")

  # If user answered yes ("y")
  if (moveOrNot == "y"){

    # Check if we're in a vignette
    # images for vignettes for R packages should be
    # in the vignettes source directory
    # So ask user what to do
    if (document_sub_dirs == "/vignettes") {

      cat("You are working in the /vignettes directory.")
      putInVignettes <- readline("Would you like to copy the image to this destination instead of /img? (y/n): ")

      if (putInVignettes == "y") {

        # Change workDir to the document_path
        # As we want to work with that folder from now on
        workDir <- document_path

        # Upcoming path of file
        new_path <- paste(workDir, base, sep="/")

      } else {

        # Create img directory if non-existent
        create_dir(workDir, "img")

        # Upcoming path of file
        new_path <- paste(workDir, "/img/", base, sep="")
      }

    } else {

      # Create img directory if non-existent
      create_dir(workDir, "img")

      # Upcoming path of file
      new_path <- paste(workDir, "/img/", base, sep="")

    }

    # Check if file already exists in img/ or vignettes/ folder
    # Move if not
    # Ask to overwrite or not if it already exists

    # Check if file at the new path already exists
    if(file.exists(new_path)){

      # If it exists
      # Ask user if it should be overwritten or not
      ovWrt <- readline("Files already exists. Overwrite? (y/n): ")

      # If yes
      if (ovWrt == "y"){

        # Copy file and overwrite existing file
        file.copy(img_source, new_path, overwrite = TRUE, recursive = FALSE,
                  copy.mode = TRUE, copy.date = TRUE)

      }

    # If the file doesn't already exist
    } else {

      # Copy file to new path
      file.copy(img_source, new_path, overwrite = FALSE, recursive = FALSE,
                copy.mode = TRUE, copy.date = TRUE)
    }

    # Update img_source
    img_source <- substr(new_path, nchar(workDir)+2, nchar(new_path))

    # Check if we're working in the working directory
    # E.g. we might work in /vignette/ folder
    # And so need to go up with ../
    # Notice though that if user said yes to copy the image
    # to the vignettes directory, this became the path of workDir!

    # Check if the document is in the
    # working directory or a subfolder
    if (workDir != document_path) {

      # If the working directory path is longer
      # than the document path
      # it means that the document is not
      # in the working directory or a subfolder
      if (nchar(workDir)>nchar(document_path)){

        warning("Document is not in the working directory or a subfolder to the working directory.")

      } else {

        # Get how many occurences of "/" there is after the working directory
        # aka. the count of subfolders to move up!
        n_sub_dirs <- countCharOccurrences("/", document_sub_dirs)

        # Add ../ to img_source for every subfolder to move up
        img_source <- paste(rep("../", n_sub_dirs), img_source, sep = "")

        }


    # If image is in vignettes directory
    } else if (exists("putInVignettes") && putInVignettes == "y"){

      # Remove "/" from img_source
      img_source <- substr(img_source, 2, nchar(img_source))
    }

  }

  # Ask user to input caption
  alt_text <- readline("Enter caption: ")

  # Insert image code
  rstudioapi::insertText(location = c(start['row'], start['column']),
                         paste("![",alt_text,"](",img_source,")", sep=""),
                         id = adc$id)

}


create_dir <- function(dir, new_dir_name){

  #
  # Create directory
  # Only suppress the warning
  # that the directory already exists
  #

  tryCatch({

    # Try to create new folder in
    # current working directory
    dir.create(file.path(dir, new_dir_name))

  }, warning = function(w){

    # If the directory already exists
    # we don't need to see the warning
    if (grepl('already exists', w$message)){

      return(NULL)

      # If the warning is from something else
      # we want to see it
    } else {

      warning(w)

    }

  })

}


# Count how many times a char is in a string
# Uli Köhler at
# https://techoverflow.net/2012/11/10/r-count-occurrences-of-character-in-string/
countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}

# Notes:
# - For each readline, create while loop checking if input is (y/n)
# - When choosing an image that is already in the project folder
#   the path should still be relative.


