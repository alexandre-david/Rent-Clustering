
readInput <- function(filePos) {
  read.table(file=filePos,skip=1, header = F, sep = "," , quote = "",
             
             colClasses = c("character", "integer", "integer", "integer", "integer",
                            "integer","integer", "integer", "integer", "integer",
                            "integer", "integer", "integer", "integer",
                            "integer", "integer", 
                            "integer", "integer", 
                            "integer", "integer", "integer",
                            "integer", "integer", 
                            "integer"),
             
             col.names= c("fullVisitorId", "nbVisits", "NbCarsSeen", "TimeOnSite", "nbEvents",
                          "nbPages","Configurator", "ProductPlan", "DescubreDacia", "Details",
                          "Financiaci", "DaciaEmpresas", "range", "ConfiguratorPreferencias",
                          "ConfiguratorVersion", "ConfiguratorOpciones", 
                          "ConfiguratorFinanciacion", "ProductplanCarvisualisator", 
                          "DealerLocator", "Lead_Ebrochure", "Lead_New_Test_drive_VN",
                          "Lead_After_sales_enquiry", "Lead_General_enquiry", 
                          "Lead_Professional_Contact"),
             comment.char = "")}
