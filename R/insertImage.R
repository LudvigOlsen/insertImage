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
  work_dir <- getwd()

  # Get directory path of document
  document_path <- dirname(adc$path)

  # Remove working directory from document_path
  # Then we know which subfolders of the wd the document is in
  document_sub_dirs <- substr(document_path, nchar(work_dir)+1,
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

  # Check if image is in project
  img_in_project <- grepl(work_dir, img_source)

  # If image is NOT in project
  if(!isTRUE(img_in_project)){

    # Ask user whether or not to copy file to project
    copy_input <- readline_while("Copy file to project? (y/n): ", c("y","Y","n","N"))

  }

  # If user answered yes ("y")
  if (exists("copy_input") && copy_input == "y"){

    # Create new path for the image
    # Update work_dir if user chooses to copy to
    # /vignettes directory
    np_wd <- create_new_path(work_dir, document_path,
                             document_sub_dirs, base,
                             img_folder='img')

    # Get new_path and work_dir from list
    new_path <- np_wd[['new_path']]
    work_dir <- np_wd[['work_dir']]

    # Check if file exists
    # If yes, ask user to overwrite or not
    # Copy image to new path
    copy_image(img_source, new_path)

    # Update img_source
    img_source <- substr(new_path, nchar(work_dir)+2, nchar(new_path))

    # Check if we're working in the working directory
    # E.g. we might work in /vignettes/ folder
    # And so need to go up with ../
    # Notice though that if user said yes to copy the image
    # to the vignettes directory, this became the path of work_dir!

    # Check if the document is in the
    # working directory or a subfolder
    if (work_dir != document_path) {

      # If the working directory path is longer
      # than the document path
      # it means that the document is not
      # in the working directory or a subfolder
      if (nchar(work_dir)>nchar(document_path)){

        warning("Document is not in the working directory or a subfolder to the working directory.")

      } else {

        # Get how many occurences of "/" there is after the working directory
        # aka. the count of subfolders to move up!
        n_sub_dirs <- count_char_occurrences("/", document_sub_dirs)

        # Add ../ to img_source for every subfolder to move up
        img_source <- paste(rep("../", n_sub_dirs), img_source, sep = "")

        }

    # If image is in vignettes directory
    } else if (exists("put_in_vignettes") && put_in_vignettes == "y"){

      # Remove "/" from img_source
      img_source <- substr(img_source, 2, nchar(img_source))
    }


  # If we don't move the file
  # Check if file is in project
  # If yes, make relative path

  # Check if work_dir is in img_source
  } else if (isTRUE(img_in_project)){

    # Remove work_dir from img_source
    img_source <- substr(img_source, nchar(work_dir)+2, nchar(img_source))

    # Check if the document is in the
    # working directory or a subfolder
    if (work_dir != document_path) {

      # Get how many occurences of "/" there is after the working directory
      # aka. the count of subfolders to move up!
      n_sub_dirs <- count_char_occurrences("/", document_sub_dirs)

      # Add ../ to img_source for every subfolder to move up
      img_source <- paste(rep("../", n_sub_dirs), img_source, sep = "")

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


create_new_path <- function(work_dir, document_path,
                            document_sub_dirs, base,
                            img_folder='img'){

  #
  # Creates new path for image
  # Checks if user is in /vignettes and so
  # if user want to use that directory for images
  # Else it creates 'img' directory in working directory
  #
  # Returns list with new path for image and the possibly changed
  # work_dir path - we update this it if user wants
  # to use /vignettes
  #

  # Check if we're in a vignette
  # images for vignettes for R packages should be
  # in the vignettes source directory
  # So ask user what to do
  if (document_sub_dirs == "/vignettes") {

    cat("You are working in the /vignettes directory.")
    put_in_vignettes <- readline_while("Would you like to copy the image to this destination instead of /img? (y/n): ",
                                       c("y","Y","n","N"))

    if (put_in_vignettes == "y") {

      # Change work_dir to the document_path
      # As we want to work with that folder from now on
      work_dir <- document_path

      # Upcoming path of file
      new_path <- paste(work_dir, base, sep="/")

      return(list('new_path' = new_path, 'work_dir' = work_dir))

    } else {

      # Create img directory if non-existent
      create_dir(work_dir, img_folder)

      # Upcoming path of file
      new_path <- paste(work_dir, "/", img_folder, "/", base, sep="")

      return(list('new_path' = new_path, 'work_dir' = work_dir))
    }

  } else {

    # Create img directory if non-existent
    create_dir(work_dir, img_folder)

    # Upcoming path of file
    new_path <- paste(work_dir, "/", img_folder, "/", base, sep="")

    return(list('new_path' = new_path, 'work_dir' = work_dir))

  }

}


copy_image <- function(img_source, new_path){

  # Check if file already exists in img/ or vignettes/ folder
  # Move if not
  # Ask to overwrite or not if it already exists

  # Check if file at the new path already exists
  if(file.exists(new_path)){

    # If it exists
    # Ask user if it should be overwritten or not
    overwrite_input <- readline_while("Files already exists. Overwrite? (y/n): ",
                            c("y","Y","n","N"))

    # If yes
    if (overwrite_input == "y"){

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

}


# Count how many times a char is in a string
# Uli Köhler at
# https://techoverflow.net/2012/11/10/r-count-occurrences-of-character-in-string/
count_char_occurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}


readline_while <- function(message, responses = c("y","n")){

  #
  # Runs readline until an allowed answer is given
  #

  # message must be character
  stopifnot(is.character(message))

  # As long as resp is not in the list of allowed responses
  # or it doesn't exist (first round)
  while(!exists("resp") || !(resp %in% responses)){

    # Ask user for input with the given message
    resp <- readline(message)

  }

  # Return accepted user input
  return(resp)

}


