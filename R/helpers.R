
#   __________________ #< bbeb9691656b900fdccfd6a7c10573e1 ># __________________
#   helper: create_dir                                                      ####

create_dir <- function(dir, new_dir_name){

##  ............................................................................
##  Description                                                             ####

  #
  # Create directory
  # Only suppress the warning
  # that the directory already exists
  #

##  ............................................................................
##  Create directory                                                        ####
### . . . . . . . . .. #< 5649527893af6b6caf134ee3837ecf5e ># . . . . . . . . ..
### Try to create directory                                                 ####

  tryCatch({

    # Try to create new folder in
    # current working directory
    dir.create(file.path(dir, new_dir_name))

### . . . . . . . . .. #< 9dc02ec76b80c017986b882729cd37cc ># . . . . . . . . ..
### Handle warning                                                          ####

  }, warning = function(w){

### . . . . . . . . .. #< a8b4cf10fca3d56b915c862278b59a45 ># . . . . . . . . ..
### - warning: dir already exists                                           ####

    # If the directory already exists
    # we don't need to see the warning
    if (grepl('already exists', w$message)){

      return(NULL)

### . . . . . . . . .. #< 8f9feb23c9382321efd3e04d4caa1c29 ># . . . . . . . . ..
### - warning: unknown                                                      ####

      # If the warning is from something else
      # we want to see it
    } else {

      warning(w)

    }

  })

}

#   __________________ #< 47b9919eea33812ffedbfe7bc9d532a7 ># __________________
#   helper: create_new_path                                                 ####

create_new_path <- function(work_dir, document_path,
                            document_sub_dirs, base,
                            img_folder='img'){


##  ............................................................................
##  Description                                                             ####

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

##  .................. #< edb0ca8dced17a328f911a1322fbd136 ># ..................
##  If document is vignette                                                 ####

  # Check if we're in a vignette
  # images for vignettes for R packages should be
  # in the vignettes source directory
  # So ask user what to do
  if (document_sub_dirs == "/vignettes") {

### . . . . . . . . .. #< bf51551d1fd5540b9bedbbcdd46c59c6 ># . . . . . . . . ..
### User input - copy img to /vignettes?                                    ####

    cat("You are working in the /vignettes directory.")
    put_in_vignettes <- readline_while("Would you like to copy the image to this destination instead of /img? (y/n): ",
                                       c("y","Y","n","N"))

### . . . . . . . . .. #< e71223a868602909a8e0abdb8dca2fc7 ># . . . . . . . . ..
### - yes                                                                   ####

    if (put_in_vignettes == "y") {

      # Change work_dir to the document_path
      # As we want to work with that folder from now on
      work_dir <- document_path

      # Upcoming path of file
      new_path <- paste(work_dir, base, sep="/")

      return(list('new_path' = new_path, 'work_dir' = work_dir))

### . . . . . . . . .. #< 528bc1a4c867e73e955464acd1e44213 ># . . . . . . . . ..
### - no                                                                    ####

    } else {

      # Create img directory if non-existent
      create_dir(work_dir, img_folder)

      # Upcoming path of file
      new_path <- paste(work_dir, "/", img_folder, "/", base, sep="")

      return(list('new_path' = new_path, 'work_dir' = work_dir))
    }

##  .................. #< c763c3a6c0b4bb637305d6d491457c3e ># ..................
##  If document is not vignette                                             ####

  } else {

    # Create img directory if non-existent
    create_dir(work_dir, img_folder)

    # Upcoming path of file
    new_path <- paste(work_dir, "/", img_folder, "/", base, sep="")

    return(list('new_path' = new_path, 'work_dir' = work_dir))

  }

}


#   __________________ #< 03d4ac80667fc794564178e5d8bb8528 ># __________________
#   helper: copy_image                                                      ####

copy_image <- function(img_source, new_path){


##  ............................................................................
##  Description                                                             ####

  # Check if file already exists in img/ or vignettes/ folder
  # Move if not
  # Ask to overwrite or not if it already exists

##  .................. #< 5e45f8134b1419ecbbe83fdf64e66618 ># ..................
##  If the file already exists                                              ####

  # Check if file at the new path already exists
  if(file.exists(new_path)){


### . . . . . . . . .. #< ebc567cb076e9aa073015a812ad71c98 ># . . . . . . . . ..
### User input - overwrite?                                                 ####

    # If it exists
    # Ask user if it should be overwritten or not
    overwrite_input <- readline_while("Files already exists. Overwrite? (y/n): ",
                                      c("y","Y","n","N"))

### . . . . . . . . .. #< 79eee446884c8540c008e88cdbe64fc7 ># . . . . . . . . ..
### - yes; copy and overwrite img file                                      ####

    # If yes
    if (overwrite_input == "y"){

      # Copy file and overwrite existing file
      file.copy(img_source, new_path, overwrite = TRUE, recursive = FALSE,
                copy.mode = TRUE, copy.date = TRUE)

    }

##  .................. #< a8674e9439527703bcf58882db534e31 ># ..................
##  If the file does not exist                                              ####

  }
  else {

### . . . . . . . . .. #< 60b5f788db42930fe0a5f40dee7626f9 ># . . . . . . . . ..
### Copy img file                                                           ####

    # Copy file to new path
    file.copy(img_source, new_path, overwrite = FALSE, recursive = FALSE,
              copy.mode = TRUE, copy.date = TRUE)
  }

}


#   __________________ #< 3059f047c3fe962415c973efb6c45e46 ># __________________
#   helper: count_char_occurences                                           ####

count_char_occurrences <- function(char, s) {

##  ............................................................................
##  Description                                                             ####

  #
  # Count how many times a char is in a string
  # Uli Köhler at
  # https://techoverflow.net/2012/11/10/r-count-occurrences-of-character-in-string/
  #

##  .................. #< 918f59c557502d39a263579fa839a984 ># ..................
##  Count char occurences                                                   ####

  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}


#   __________________ #< 8c66ba4a55ee333fb835515a5fcf37fa ># __________________
#   helper: readline_while                                                  ####

readline_while <- function(message, responses = c("y","n")){

##  ............................................................................
##  Description                                                             ####

  #
  # Runs readline until an allowed answer is given
  #

##  .................. #< afa40bc764f547d75453ddab2af4aac9 ># ..................
##  Argument checks                                                         ####

  # message must be character
  stopifnot(is.character(message))

##  .................. #< 3852eee207dff65eb97bd39333303c3d ># ..................
##  readline() until acceptable answer                                      ####

  # As long as resp is not in the list of allowed responses
  # or it doesn't exist (first round)
  while(!exists("resp") || !(resp %in% responses)){

    # Ask user for input with the given message
    resp <- readline(message)

  }

##  .................. #< b8961aac26b8c9a70ef5589e5189e455 ># ..................
##  Return response                                                         ####

  # Return accepted user input
  return(resp)

}
