#For use with all raw EQAO ISD file
# 
#Use:   df <- Re.Gender(df)
#
#Gender is a standard field used in all ISDs and it not assessment specific

Re.Gender <- function(x){
  x$re.gender <- ifelse(x$Gender == "-1", "Missing",
                          ifelse(x$Gender == "1", "Male",
                                 ifelse(x$Gender == "2", "Female", "BadCode")
                          )
         )
return(x) 
}

