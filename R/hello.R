#' @title hello
#'
#' @description This is our hello world function!
#'
#' @param first character The first name to say hello to.
#' @param last character the last name to say hello to.
#'
#' @export
#'
#' @examples #put a basic example above the function
#' hello("Laura")
#' hello("Laura","OWI")
#' hello(last="OWI", first = "Laura")
hello <- function(first,last="Userbot") {

  first_letter <- substr(first, 1,1)
  first_letter <- tolower(first_letter)

  first_number <- lookup_thingie$x[lookup_thingie$s == first_letter]

  print(paste(first_number,"Hello",first, last,"!"))
}
