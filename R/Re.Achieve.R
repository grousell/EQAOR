# Function to recode achievement levels in Primary, Junior, Grade 9 and OSSLT
# Primary/Junior/Grade 9 - Converts 1, 2, 3, 4 to Level 1, Level 2, Level 3, Level 4
# OSSLT - Recodes to Successful, Unsuccessful,


re.Achieve <- function (x) {
  ifelse("OSSLTOutcome" %in% colnames(x),
         {x$Outcome <- ifelse (x$OSSLTOutcome == "0", "Pending",
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
         x$Prior_G6_R <- ifelse (x$Prior_G6_ROverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G6_ROverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G6_ROverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G6_ROverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G6_ROverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G6_ROverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G6_ROverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G6_ROverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G6_ROverallLevel == "X", "Exempt", "NA")
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
         )
         x$Prior_G6_W <- ifelse (x$Prior_G6_WOverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G6_WOverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G6_WOverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G6_WOverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G6_WOverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G6_WOverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G6_WOverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G6_WOverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G6_WOverallLevel == "X", "Exempt", "NA")
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
         )
         x$Prior_G3_R <- ifelse (x$Prior_G3_ROverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G3_ROverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G3_ROverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G3_ROverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G3_ROverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G3_ROverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G3_ROverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G3_ROverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G3_ROverallLevel == "X", "Exempt", "NA")
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
         )
         x$Prior_G3_W <- ifelse (x$Prior_G3_WOverallLevel == "-1", "No Information",
                                 ifelse (x$Prior_G3_WOverallLevel == "0", "NE1",
                                         ifelse (x$Prior_G3_WOverallLevel == "1", "Level 1",
                                                 ifelse (x$Prior_G3_WOverallLevel == "2", "Level 2",
                                                         ifelse (x$Prior_G3_WOverallLevel == "3", "Level 3",
                                                                 ifelse (x$Prior_G3_WOverallLevel == "4", "Level 4",
                                                                         ifelse (x$Prior_G3_WOverallLevel == "B", "No Data",
                                                                                 ifelse (x$Prior_G3_WOverallLevel == "W", "Withheld",
                                                                                         ifelse (x$Prior_G3_WOverallLevel == "X", "Exempt", "NA")
                                                                                 )
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
         )
         },
         ifelse ("MathClassWhen" %in% colnames(x),
                 {x$MathLevel <- ifelse (x$OverallOutcomeLevel == "0", "NE1",
                                  ifelse (x$OverallOutcomeLevel == "1", "Level 1",
                                          ifelse (x$OverallOutcomeLevel == "2", "Level 2",
                                                  ifelse (x$OverallOutcomeLevel == "3", "Level 3",
                                                          ifelse (x$OverallOutcomeLevel == "4", "Level 4",
                                                                  ifelse (x$OverallOutcomeLevel == "B", "No Data",
                                                                          ifelse (x$OverallOutcomeLevel == "V", "Vulgar, Obscene", "Withheld")))))))

          x$Prior_G6_Read <- ifelse (x$Prior_G6_ROverallLevel == "-1", "No information",
                                     ifelse (x$Prior_G6_ROverallLevel == "0", "NE1",
                                             ifelse(x$Prior_G6_ROverallLevel == "1", "Level 1",
                                                    ifelse(x$Prior_G6_ROverallLevel == "2", "Level 2",
                                                           ifelse (x$Prior_G6_ROverallLevel == "3", "Level 3",
                                                                   ifelse (x$Prior_G6_ROverallLevel == "4", "Level 4",
                                                                           ifelse (x$Prior_G6_ROverallLevel == "B", "No Data",
                                                                                   ifelse (x$Prior_G6_ROverallLevel == "P", "Pending",
                                                                                           ifelse (x$Prior_G6_ROverallLevel == "X", "Exempt", "Withheld")))))))))
          x$Prior_G6_Write <- ifelse (x$Prior_G6_WOverallLevel == "-1", "No information",
                                      ifelse (x$Prior_G6_WOverallLevel == "0", "NE1",
                                              ifelse(x$Prior_G6_WOverallLevel == "1", "Level 1",
                                                     ifelse(x$Prior_G6_WOverallLevel == "2", "Level 2",
                                                            ifelse (x$Prior_G6_WOverallLevel == "3", "Level 3",
                                                                    ifelse (x$Prior_G6_WOverallLevel == "4", "Level 4",
                                                                            ifelse (x$Prior_G6_WOverallLevel == "B", "No Data",
                                                                                    ifelse (x$Prior_G6_WOverallLevel == "P", "Pending",
                                                                                            ifelse (x$Prior_G6_WOverallLevel == "X", "Exempt", "Withheld")))))))))

          x$Prior_G6_M <- ifelse (x$Prior_G6_MOverallLevel == "-1", "No information",
                                  ifelse (x$Prior_G6_MOverallLevel == "0", "NE1",
                                          ifelse(x$Prior_G6_MOverallLevel == "1", "Level 1",
                                                 ifelse(x$Prior_G6_MOverallLevel == "2", "Level 2",
                                                        ifelse (x$Prior_G6_MOverallLevel == "3", "Level 3",
                                                                ifelse (x$Prior_G6_MOverallLevel == "4", "Level 4",
                                                                        ifelse (x$Prior_G6_MOverallLevel == "B", "No Data",
                                                                                ifelse (x$Prior_G6_MOverallLevel == "P", "Pending",
                                                                                        ifelse (x$Prior_G6_MOverallLevel == "X", "Exempt", "Withheld")))))))))

          x$Prior_G3_R <- ifelse (x$Prior_G3_ROverallLevel == "-1", "No information",
                                  ifelse (x$Prior_G3_ROverallLevel == "0", "NE1",
                                          ifelse(x$Prior_G3_ROverallLevel == "1", "Level 1",
                                                 ifelse(x$Prior_G3_ROverallLevel == "2", "Level 2",
                                                        ifelse (x$Prior_G3_ROverallLevel == "3", "Level 3",
                                                                ifelse (x$Prior_G3_ROverallLevel == "4", "Level 4",
                                                                        ifelse (x$Prior_G3_ROverallLevel == "B", "No Data",
                                                                                ifelse (x$Prior_G3_ROverallLevel == "P", "Pending",
                                                                                        ifelse (x$Prior_G3_ROverallLevel == "X", "Exempt", "Withheld")))))))))

          x$Prior_G3_W <- ifelse (x$Prior_G3_WOverallLevel == "-1", "No information",
                                  ifelse (x$Prior_G3_WOverallLevel == "0", "NE1",
                                          ifelse(x$Prior_G3_WOverallLevel == "1", "Level 1",
                                                 ifelse(x$Prior_G3_WOverallLevel == "2", "Level 2",
                                                        ifelse (x$Prior_G3_WOverallLevel == "3", "Level 3",
                                                                ifelse (x$Prior_G3_WOverallLevel == "4", "Level 4",
                                                                        ifelse (x$Prior_G3_WOverallLevel == "B", "No Data",
                                                                                ifelse (x$Prior_G3_WOverallLevel == "P", "Pending",
                                                                                        ifelse (x$Prior_G3_WOverallLevel == "X", "Exempt", "Withheld")))))))))

          x$Prior_G3_M <- ifelse (x$Prior_G3_MOverallLevel == "-1", "No information",
                                  ifelse (x$Prior_G3_MOverallLevel == "0", "NE1",
                                          ifelse(x$Prior_G3_MOverallLevel == "1", "Level 1",
                                                 ifelse(x$Prior_G3_MOverallLevel == "2", "Level 2",
                                                        ifelse (x$Prior_G3_MOverallLevel == "3", "Level 3",
                                                                ifelse (x$Prior_G3_MOverallLevel == "4", "Level 4",
                                                                        ifelse (x$Prior_G3_MOverallLevel == "B", "No Data",
                                                                                ifelse (x$Prior_G3_MOverallLevel == "P", "Pending",
                                                                                        ifelse (x$Prior_G3_MOverallLevel == "X", "Exempt", "Withheld")))))))))

          },
                ifelse (x$Grade == "6",
                  {x$ReadLevel  <- ifelse (x$ROverallLevel == "0", "NE1",
                                          ifelse (x$ROverallLevel == "1", "Level 1",
                                                  ifelse (x$ROverallLevel == "2", "Level 2",
                                                          ifelse (x$ROverallLevel == "3", "Level 3",
                                                                  ifelse (x$ROverallLevel == "4", "Level 4",
                                                                          ifelse (x$ROverallLevel == "B", "No Data",
                                                                                  ifelse (x$ROverallLevel == "P", "Results Pending",
                                                                                          ifelse (x$ROverallLevel == "Q", "Not required to write",
                                                                                                  ifelse (x$ROverallLevel == "X", "Exempt", "NA")))))))))
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
                                         )

                 x$G3Read <- ifelse (x$Prior_G3_ROverallLevel == "-1", "No information",
                                     ifelse (x$Prior_G3_ROverallLevel == "0", "NE1",
                                             ifelse(x$Prior_G3_ROverallLevel == "1", "Level 1",
                                                    ifelse (x$Prior_G3_ROverallLevel == "2", "Level 2",
                                                            ifelse (x$Prior_G3_ROverallLevel == "3", "Level 3",
                                                                    ifelse (x$Prior_G3_ROverallLevel == "4", "Level 4",
                                                                            ifelse (x$Prior_G3_ROverallLevel == "B", "No Data",
                                                                                    ifelse (x$Prior_G3_ROverallLevel == "P", "Pending",
                                                                                            ifelse (x$Prior_G3_ROverallLevel == "Q", "Not requried to write",
                                                                                                    ifelse (x$Prior_G3_ROverallLevel == "W", "Withheld",
                                                                                                            ifelse (x$Prior_G3_ROverallLevel == "R", "Withheld",
                                                                                                                    ifelse (x$Prior_G3_ROverallLevel == "X", "Exempt", "Grade 6 File Only")
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

                 x$G3Write <- ifelse (x$Prior_G3_WOverallLevel == "-1", "No information",
                                     ifelse (x$Prior_G3_WOverallLevel == "0", "NE1",
                                             ifelse(x$Prior_G3_WOverallLevel == "1", "Level 1",
                                                    ifelse (x$Prior_G3_WOverallLevel == "2", "Level 2",
                                                            ifelse (x$Prior_G3_WOverallLevel == "3", "Level 3",
                                                                    ifelse (x$Prior_G3_WOverallLevel == "4", "Level 4",
                                                                            ifelse (x$Prior_G3_WOverallLevel == "B", "No Data",
                                                                                    ifelse (x$Prior_G3_WOverallLevel == "P", "Pending",
                                                                                            ifelse (x$Prior_G3_WOverallLevel == "Q", "Not requried to write",
                                                                                                    ifelse (x$Prior_G3_WOverallLevel == "W", "Withheld",
                                                                                                            ifelse (x$Prior_G3_WOverallLevel == "R", "Withheld",
                                                                                                                    ifelse (x$Prior_G3_WOverallLevel == "X", "Exempt", "Grade 6 File Only")
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

                 x$G3Math <- ifelse (x$Prior_G3_MOverallLevel == "-1", "No information",
                                     ifelse (x$Prior_G3_MOverallLevel == "0", "NE1",
                                             ifelse(x$Prior_G3_MOverallLevel == "1", "Level 1",
                                                    ifelse (x$Prior_G3_MOverallLevel == "2", "Level 2",
                                                            ifelse (x$Prior_G3_MOverallLevel == "3", "Level 3",
                                                                    ifelse (x$Prior_G3_MOverallLevel == "4", "Level 4",
                                                                            ifelse (x$Prior_G3_MOverallLevel == "B", "No Data",
                                                                                    ifelse (x$Prior_G3_MOverallLevel == "P", "Pending",
                                                                                            ifelse (x$Prior_G3_MOverallLevel == "Q", "Not requried to write",
                                                                                                    ifelse (x$Prior_G3_MOverallLevel == "W", "Withheld",
                                                                                                            ifelse (x$Prior_G3_MOverallLevel == "R", "Withheld",
                                                                                                                    ifelse (x$Prior_G3_MOverallLevel == "X", "Exempt", "Grade 6 File Only")
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


                 },
                      ifelse (x$Grade == "3",
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
                         )

                         },
                         x$ERROR <- "Unknown File Format")
                 )
          )
         )

  return (x)

}

