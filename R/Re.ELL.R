#For use with all raw EQAO ISD file
#
#Use:   df <- Re.ELL(df)
#
#Function determines whether the ISD is elementary or secondary assessments and recodes

Re.ELL <- function(x){
  ifelse("Background_ESLELD_ALFPDF" %in% colnames(x),
    {x$ELL_R <- ifelse(x$Background_ESLELD_ALFPDF == "-1", "Missing",
                        ifelse(x$Background_ESLELD_ALFPDF == "0", "Not ELL",
                               ifelse(x$Background_ESLELD_ALFPDF == "1", "ELL",
                                      ifelse(x$Background_ESLELD_ALFPDF == "2", NULL,
                                             ifelse(x$Background_ESLELD_ALFPDF == "3", NULL,"BadCode"
                                             )
                                      )
                               )
                        )
  )},
    ifelse("ESLELD_ALFPDF" %in% colnames(x),
        {x$ELL_R <- ifelse(x$ESLELD_ALFPDF == "-1", "Missing",
                          ifelse(x$ESLELD_ALFPDF == "0", "Not ELL",
                                 ifelse(x$ESLELD_ALFPDF == "1", "ELL",
                                        ifelse(x$ESLELD_ALFPDF == "2", NULL,
                                               ifelse(x$ESLELD_ALFPDF == "3", NULL,"BadCode"
                                               )
                                        )
                                 )
                          )
       )},  x$ELL_R <- "Unknown File Format"
    )
)
  return(x)
}
