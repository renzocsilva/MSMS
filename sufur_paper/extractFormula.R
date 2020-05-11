f.extract <- function(formula) {
  first <- "^([[:upper:]][[:lower:]]?)([0-9]*).*" 
  last <- "^[[:upper:]][[:lower:]]?[0-9]*(.*)" 
  result <- list() 
  extract <- formula 
  while ((start <- nchar(extract)) > 0){
    chem <- sub(first, '\\1 \\2', extract) 
    extract <- sub(last, '\\1', extract) 
      if (nchar(extract) == start){
        warning("Invalid formula:", formula) 
        return(NULL) 
        } 
    result[[length(result) + 1L]] <- strsplit(chem, ' ')[[1]] 
    } 
    result 
} 

z <- as.data.frame(f.extract("C5H11BrO"))


form <- "C5H11BrO" 
ups <- c(gregexpr("[[:upper:]]", form)[[1]], nchar(form) + 1) 
seperated <- sapply(1:(length(ups)-1), function(x) substr(form, ups[x], ups[x+1] - 1)) 
elements <-  gsub("[[:digit:]]", "", seperated) 
nums <- gsub("[[:alpha:]]", "", seperated) 
ans <- data.frame(element = as.character(elements), 
num <- as.numeric(ifelse(nums == "", 1, nums)), stringsAsFactors = FALSE)
x<- as.data.frame(num)
names(x)<-elements
