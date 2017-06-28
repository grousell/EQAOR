# Function to recode achievement levels in Primary, Junior, Grade 9 and OSSLT
# Primary/Junior/Grade 9 - Converts 1, 2, 3, 4 to Level 1, Level 2, Level 3, Level 4
# OSSLT - Recodes to Successful, Unsuccessful,

achieveR <- function (x) {
  ifelse("OSSLTOutcome" %in% colnames(x),
         {x$Outcome <- ifelse (x$OSSLTOutcome == "0", "Pending",
                               ifelse (x$OSSLTOutcome == "1", "Successful",
                                       ifelse (x$OSSLTOutcome == "2", "Unsuccessful",
                                               ifelse (x$OSSLTOutcome == "3", "Absent",
                                                       ifelse (x$OSSLTOutcome == "4", "OSSLC",
                                                               ifelse (x$OSSLTOutcome == "6", "Exempt","Withheld")
                                                               )
                                                       )
                                               )
                                       )
                               )
         },
         ifelse ("ROverallLevel" %in% colnames(x),
                 {x$ReadLevel  <- ifelse (x$ROverallLevel == "0", "NE1",
                                          ifelse (x$ROverallLevel == "1", "Level 1",
                                                  ifelse (x$ROverallLevel == "2", "Level 2",
                                                          ifelse (x$ROverallLevel == "3", "Level 3",
                                                                  ifelse (x$ROverallLevel == "4", "Level 4",
                                                                          ifelse (x$ROverallLevel == "B", "No Data",
                                                                                  ifelse (x$ROverallLevel == "P", "Results Pending",
                                                                                          ifelse (x$ROverallLevel == "Q", "Not required to write",
                                                                                                  ifelse (x$ROverallLevel == "X", "Exempt", "NA")
                                                                                                  )
                                                                                          )
                                                                                  )
                                                                          )
                                                                  )
                                                          )
                                                  )
                                          )
                 x$WriteLevel  <- ifelse (x$WOverallLevel == "0", "NE1",
                                          ifelse (x$WOverallLevel == "1", "Level 1",
                                                  ifelse (x$WOverallLevel == "2", "Level 2",
                                                          ifelse (x$WOverallLevel == "3", "Level 3",
                                                                  ifelse (x$WOverallLevel == "4", "Level 4",
                                                                          ifelse (x$WOverallLevel == "B", "No Data",
                                                                                  ifelse (x$WOverallLevel == "P", "Results Pending",
                                                                                          ifelse (x$WOverallLevel == "Q", "Not required to write",
                                                                                                  ifelse (x$WOverallLevel == "X", "Exempt", "NA")
                                                                                          )
                                                                                  )
                                                                          )
                                                                  )
                                                          )
                                                  )
                                          )
                 )
                 x$MathLevel  <- ifelse (x$MOverallLevel == "0", "NE1",
                                         ifelse (x$MOverallLevel == "1", "Level 1",
                                                 ifelse (x$MOverallLevel == "2", "Level 2",
                                                         ifelse (x$MOverallLevel == "3", "Level 3",
                                                                 ifelse (x$MOverallLevel == "4", "Level 4",
                                                                         ifelse (x$MOverallLevel == "B", "No Data",
                                                                                 ifelse (x$MOverallLevel == "P", "Results Pending",
                                                                                         ifelse (x$MOverallLevel == "Q", "Not required to write",
                                                                                                 ifelse (x$MOverallLevel == "X", "Exempt", "NA")
                                                                                         )
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                 )},
                 ifelse ("MathClassWhen" %in% colnames(x),
                         {x$MathLevel <- ifelse (x$OverallOutcomeLevel == "0", "NE1",
                                                 ifelse (x$OverallOutcomeLevel == "1", "Level 1",
                                                         ifelse (x$OverallOutcomeLevel == "2", "Level 2",
                                                                 ifelse (x$OverallOutcomeLevel == "3", "Level 3",
                                                                         ifelse (x$OverallOutcomeLevel == "4", "Level 4",
                                                                                 ifelse (x$OverallOutcomeLevel == "B", "No Data",
                                                                                         ifelse (x$OverallOutcomeLevel == "V", "Vulgar, Obscene", "Withheld")
                                                                                         )
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                         },
                         x$ERROR <- "Unknown File Format"
                         )
                 )
         )

  return (x)

}

