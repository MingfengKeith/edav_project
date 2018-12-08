library(tidyverse)
setwd("/Users/hantangzhou/OneDrive/Columbia University in the City of New York/STAT GR5702/Final Project/edav_project")
data <- read_csv('Rodent_Inspection.csv')

clean = 0
if(clean == 1){
  data <- data %>% select(-JOB_TICKET_OR_WORK_ORDER_ID, -JOB_ID, -LOCATION, -JOB_PROGRESS, -BOROUGH, -X_COORD, 
                        -Y_COORD, -APPROVED_DATE, -ID)

  data <- data %>% select(-BBL, -LOT, -BLOCK, -HOUSE_NUMBER, -STREET_NAME, -RESULT)

  data <- na_if(data, 0)
  data <- na_if(data, 111111)

  data <- data %>% drop_na()

  data <- tibble::rowid_to_column(data, "ID")

  sample <- sample_n(data, 0.2 * nrow(data))

  write_csv(sample, "Rodent_Inspection_Lite.csv")
}

chartTest <- function(data, chart_type){
  # distinct zip and distinct year
  zip_list <- as.list(data$ZIP_CODE)
  zip_list <- unique(zip_list)
  year_list <- format(as.Date(data$INSPECTION_DATE, format="%d/%m/%Y"),"%Y")
  year_list <- unique(year_list)
  
  for(year in year_list){
    year_data <- data %>% filter( format(as.Date(INSPECTION_DATE, format="%d/%m/%Y"),"%Y") == year)
    for(zip in zip_list){
      year_zip_data <- year_data %>% filter(ZIP_CODE == zip)
      filename <- ""
      filename <- paste(chart_type, toString(zip), sep = "_")
      filename <- paste(filename, toString(year), sep="_")
      filename <- paste(filename, ".jpg", sep="")
      
      jpeg(filename)
      
      ggplot(data=year_zip_data, aes(x=INSPECTION_TYPE)) + geom_bar()
      
      dev.off()
    }
  }
}

chartTest(data, "hist")

test <- function(yzd){
  jpeg("filename.jpg")
  ggplot(data=yzd, aes(x=INSPECTION_TYPE)) + geom_bar()
  dev.off()
}
test(year_zip_data)

num <- c(1,2)
for(i in num){
jpeg("filename.jpg")
ggplot(data=year_zip_data, aes(x=INSPECTION_TYPE)) + geom_bar()
dev.off()
}
