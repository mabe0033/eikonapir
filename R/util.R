# changed by Martin Becker
is_string <- function (value) is.character(value) && (length(value) == 1)

is_named_list <- function (value) is.list(value) && (!is.null(names(value)))

is_non_named_list <- function (value) is.list(value) && is.null(names(value))

sanitize_string <- function(value,name) {
	if (is.null(value)) NULL else {
		value <- as.character(value)
		if (is.character(value)&&(length(value)==1)) value else {
			warning(paste0(name," must be a string"))
			NULL
		}
  }
}

sanitize_integer <- function(value,name) {
	if (is.null(value)) NULL else {
		value <- as.integer(value)
		if (is.integer(value)&&(length(value)==1)) value else {
			warning(paste0(name," must be an integer"))
			NULL
		}
  }
}

fixDate <- function(date,start=TRUE) {
  date <- sanitize_string(date)
  if (nchar(date)==10) paste0(date,if (start) "T00:00:00" else "T23:59:59") else
    date	
}

