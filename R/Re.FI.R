#For use with all raw EQAO ISD file
# 
#Use:   df <- Re.FI(df)
#
#French Immersion is an elementary distinction.  Secondary is addressed through course types
#
#Function determines whether the ISD is elementary or secondary assessments and recodes

Re.FI <- function(x){
  ifelse("Background_FrenchImmersion" %in% colnames(x),
    {x$re.fi <- ifelse(x$Background_FrenchImmersion == "-1", "Missing",
                        ifelse(x$Background_FrenchImmersion == "0", "Not FI",
                               ifelse(x$Background_FrenchImmersion == "1", "FI (A)",
                                      ifelse(x$Background_FrenchImmersion == "2", "FI (B)",
                                             ifelse(x$Background_FrenchImmersion == "3", "FI (C)",
                                                    ifelse(x$Background_FrenchImmersion == "4", "FI (G6)", "BadCode"
                                                    )
                                             )
                                      )
                               )
                        )
       )},  x$re.fi <- "Secondary - Not Applicable"
  )
  return(x) 
}
