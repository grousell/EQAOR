#For use with all raw EQAO ISD file
# 
#Use:   df <- Re.Course(df)
#
#Course Type is a secondary distinction.  
#
#Function determines whether the ISD is elementary or secondary assessments and recodes

Re.Course <- function(x){
  ifelse("LevelOfStudyLanguage" %in% colnames(x),
    {x$re.course <- ifelse(x$LevelOfStudyLanguage == "-2", "Ambiguous",
                        ifelse(x$LevelOfStudyLanguage == "-1", "Missing",
                               ifelse(x$LevelOfStudyLanguage == "0", "NA",
                                      ifelse(x$LevelOfStudyLanguage == "1", "Academic",
                                             ifelse(x$LevelOfStudyLanguage == "2", "Applied",
                                                    ifelse(x$LevelOfStudyLanguage == "3", "Locally Developed",
                                                           ifelse(x$LevelOfStudyLanguage == "4", "ESL/ELD",
                                                                  ifelse(x$LevelOfStudyLanguage == "5", "Other","BadCode")
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
    )}, 
    ifelse("Program" %in% colnames(x),
           {x$re.course <- ifelse(x$Program =="1", "Applied",
                                  ifelse(x$Program == "2", "Academic", "BadCode")
           )}, x$re.course <- "Elementary - Not Applicable"
    )
  )
    return(x)
}
