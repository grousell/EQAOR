
# Function to Recode OSSLT ISD Files --------------------------------------
# Creates the following columns with recoded data
# IEPcode - All IEP designations into single column
# Outcome - Successful, Uncucessful, etc.
# GenderR - Male/Female
# Eligibility - FTE/PE
# Level of Study - Academic/Applied, etc.
# ESL - Yes/No
# Prior_G6_R - Level 1, Level 2, etc.
# Prior_G6_W - Level 1, Level 2, etc.
# Prior_G3_R - Level 1, Level 2, etc.
# Prior_G3_W - Level 1, Level 2, etc.

# Use:
# df <- OSSLT.Recode (df)

Re.OSSLT <- function (x) {
  ifelse("OSSLTOutcome" %in% colnames(x),
         {x$IEPcode <- paste0(x$IEP,
                              x$IPRCExBehaviour,
                              x$IPRCExAutism,
                              x$IPRCExDeaf,
                              x$IPRCExLanguage,
                              x$IPRCExSpeech,
                              x$IPRCExLearning,
                              x$IPRCExGiftedness,
                              x$IPRCExMildIntellectual,
                              x$IPRCExDevelopmental,
                              x$IPRCExPhysical,
                      x$IPRCExBlind,
                      x$IPRCExMultiple)
         x$IEPcode <- ifelse(x$IEPcode == "0000000000000", "No IEP",
                             ifelse(x$IEPcode == "1000000000000", "IEP no IPRC",
                                    ifelse(x$IEPcode == "1100000000000", "Behaviour",
                                           ifelse(x$IEPcode == "1010000000000", "Autism",
                                                  ifelse(x$IEPcode == "1001000000000", "Deaf",
                                                         ifelse(x$IEPcode == "1000100000000", "Language",
                                                                ifelse(x$IEPcode == "1000010000000", "Speech",
                                                                       ifelse(x$IEPcode == "1000001000000","Learning",
                                                                              ifelse(x$IEPcode == "1000000100000","Giftedness",
                                                                                     ifelse(x$IEPcode == "1000000010000","MildIntellectual",
                                                                                            ifelse(x$IEPcode == "1000000001000","Developmental",
                                                                                                   ifelse(x$IEPcode == "1000000000100","Physical",
                                                                                                          ifelse(x$IEPcode == "1000000000010","Blind",
                                                                                                                 ifelse(x$IEPcode == "1000000000001","Multiple","BadCode")
                                                                                                                 )
                                                                                                          )
                                                                                                   )
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
         x$Outcome <- ifelse (x$OSSLTOutcome == "0", "Pending",
                              ifelse (x$OSSLTOutcome == "1", "Successful",
                                      ifelse (x$OSSLTOutcome == "2", "Unsuccessful",
                                              ifelse (x$OSSLTOutcome == "3", "Absent",
                                                      ifelse (x$OSSLTOutcome == "4", "OSSLC",
                                                              ifelse (x$OSSLTOutcome == "5", "Deferred",
                                                                      ifelse (x$OSSLTOutcome == "6", "Exempt","Withheld")
                                                                      )
                                                              )
                                                      )
                                              )
                                      )
                              )

         x$GenderR <- ifelse (x$Gender == "1", "Male",
                              ifelse (x$Gender == "2", "Female", "NA"))

         x$Eligibility <- ifelse (x$EligibilityStatus == "-2", "Ambiguous",
                                  ifelse (x$EligibilityStatus == "-1", "Missing",
                                          ifelse (x$EligibilityStatus == "0", "Not Filled",
                                                  ifelse (x$EligibilityStatus == "1", "FTE", "PE")
                                                  )
                                          )
                                  )
         x$LevelStudy <- ifelse (x$LevelOfStudyLanguage == "-2", "Ambiguous",
                                 ifelse (x$LevelOfStudyLanguage == "-1", "Missing",
                                         ifelse(x$LevelOfStudyLanguage == "0", "Not Applicable",
                                                ifelse (x$LevelOfStudyLanguage == "1", "Academic",
                                                        ifelse (x$LevelOfStudyLanguage == "2", "Applied",
                                                                ifelse (x$LevelOfStudyLanguage == "3", "Locally Developed",
                                                                        ifelse (x$LevelOfStudyLanguage == "4", "ESL/ELD", "Other")
                                                                        )
                                                                )
                                                        )
                                                )
                                         )
                                 )
         x$ESL <- ifelse (x$ESLELD_ALFPDF == "-2", "Ambiguous",
                          ifelse (x$ESLELD_ALFPDF == "-1", "Missing",
                                  ifelse (x$ESLELD_ALFPDF == "0", "No",
                                          ifelse (x$ESLELD_ALFPDF == "1", "Yes", "NA")
                                          )
                                  )
                          )

         x$Prior_G6_Read <- ifelse (x$Prior_G6_ROverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G6_ROverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G6_ROverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G6_ROverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G6_ROverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G6_ROverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G6_ROverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G6_ROverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G6_ROverallLevel == "X", "Exempt",
                                                                                                 ifelse (x$Prior_G6_ROverallLevel == "Q", "No Information", "NA")
                                                                                         )
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
                                 )
         x$Prior_G6_Write <- ifelse (x$Prior_G6_WOverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G6_WOverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G6_WOverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G6_WOverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G6_WOverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G6_WOverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G6_WOverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G6_WOverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G6_WOverallLevel == "X", "Exempt",
                                                                                                 ifelse (x$Prior_G6_WOverallLevel == "Q", "No Information", "NA")
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
                                 )
                                 )
         x$Prior_G3_Read <- ifelse (x$Prior_G3_ROverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G3_ROverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G3_ROverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G3_ROverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G3_ROverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G3_ROverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G3_ROverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G3_ROverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G3_ROverallLevel == "X", "Exempt",
                                                                                                 ifelse (x$Prior_G3_ROverallLevel == "Q", "No Information", "NA")
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
                                 )
                                 )
         x$Prior_G3_Write <- ifelse (x$Prior_G3_WOverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G3_WOverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G3_WOverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G3_WOverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G3_WOverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G3_WOverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G3_WOverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G3_WOverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G3_WOverallLevel == "X", "Exempt",
                                                                                                 ifelse (x$Prior_G3_WOverallLevel == "Q", "No Information", "NA")
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
                                 )
         )

         },  x$ERROR <- "Incorrect File"
  )
  return(x)
}




