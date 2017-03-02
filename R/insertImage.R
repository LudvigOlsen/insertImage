
#   ____________________________________________________________________________
#   Addin function insertImage()                                            ####

#' @title Insert image.
#' @description Addin for RStudio for inserting an image into Markdown.
#'
#' Copy the image file to the project in img/ or /vignette directory.
#'
#' Uses relative paths for image files in project and absolute paths for images not in project.
#'
#' See \code{details} for setting up key command.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @return Inserts \code{![alt](path/to/file)}. Optionally copies file to project.
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

##  ............................................................................
##  Description                                                             ####

  #
  # Let's user insert image with file chooser
  # Allows copying image to project in /img/ OR
  # allows copying image to /vignettes/ if working in that
  # Let's user add alternative text via console
  #

##  .................. #< b60a8fe2ea154247f9eca1814ce48b16 ># ..................
##  Get context                                                             ####

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


##  ............................................................................
##  Get image                                                               ####
### . . . . . . . . .. #< b63e699ff90d1dc99e9213ea7b486ce0 ># . . . . . . . . ..
### Choose image file with file chooser                                     ####

  # Choose image
  # opens browser
  img_source <- file.choose()

### . . . . . . . . .. #< b27b17258866d4cca7cb572249f8d095 ># . . . . . . . . ..
### Check file                                                              ####

  # Get filename
  base <- basename(img_source)

  # Check if file is png or jpg
  # Else throw warning and continue
  if (!substr(base, nchar(base)-2,
              nchar(base)) %in%
      c("png","PNG", "jpg", "JPG")){

    warning("File doesn\'t seem to be png or jpg image")

  }

  # Check if image file is in project
  img_in_project <- grepl(work_dir, img_source)

##  ............................................................................
##  Copy Image                                                              ####
### . . . . . . . . .. #< cab9ba97fb978bff771b9c0f539eba5f ># . . . . . . . . ..
### User input - copy image?                                                ####

  # If image is NOT in project
  if(!isTRUE(img_in_project)){

    # Ask user whether or not to copy file to project
    copy_input <- readline_while("Copy file to project? (y/n): ",
                                 c("y","Y","n","N"))

  }

### . . . . . . . . .. #< 4f5087d140913429edb2409d9c6acaf0 ># . . . . . . . . ..
### Yes                                                                     ####

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

### . . . . . . . . .. #< 99095bceac7615ed43cee1d38507e36f ># . . . . . . . . ..
### No                                                                      ####

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

##  .................. #< 8fde521cb673efd21026924967608532 ># ..................
##  User input - alternative text                                           ####

  # Ask user to input caption
  alt_text <- readline("Enter caption: ")

##  .................. #< 4b18020deae154a06e0abe03b1cb3d84 ># ..................
##  Insert image code to markdown                                           ####

  # Insert image code
  rstudioapi::insertText(location = c(start['row'], start['column']),
                         paste("![",alt_text,"](",img_source,")", sep=""),
                         id = adc$id)

}





