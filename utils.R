# Utility Functions

string.trim <- function(vstring, limit=60) {
  ## Given a vector of strings (i.e. column from a resultset), trim the string
  ## at length 'limit' and append a '...'.
  new.vector <- c()
  for (f in vstring) {
    if (!is.na(f) && nchar(f) != 0 && nchar(f) > limit)
      new.vector <- c(new.vector, paste0(strtrim(f, limit-3), '...'))
    else
      new.vector <- c(new.vector, f)
  }
  return(new.vector)
}

vector.to.table <- function(data, nrow, colname) {
  ## Splits a vector into columns for displaying in a Markdown file using 
  ## the 'pander' library. The 'nrow' argument is used as split index.
  ## The 'colname' argument is used to name the created columns
  table <- split(data, ceiling(seq_along(data)/nrow))
  table <- lapply(table, function(column) {
      # Check the number of rows in the column
      missing.elements <- nrow - length(column)
      if (missing.elements > 0)
        column <- c(column, rep('', missing.elements))
      else
        return(column)
    })

  max.rows <- max(unlist(lapply(table, FUN=length)))
  attributes(table) = list(names = rep(colname, length(table)),
    row.names=1:max.rows, class='data.frame')
  
  return(table)
}

## Week / chapter number
chapter <- 1
## Question number; global var updated with each question
n.q <- 0

insert.q <- function(q_string="") {
  ## Updates the question number each time a question is added
  n.q <<- n.q + 1
  if (q_string != "")
    cat(paste0("### Question ", chapter, ".", n.q, ": ", q_string))
  else
    cat(paste0("### Question ", chapter, ".", n.q))
}