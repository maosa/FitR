library(rsconnect)
rsconnect::deployApp(appDir = paste0(Sys.getenv("HOME"), "/fitr"))