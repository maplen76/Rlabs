setwd("F:\\network_analysis")

library(jsonlite)
library(qdap)
library(tidyverse)
library(rgeolocate)
library(openxlsx)
# file.edit('~/.Renviron') to edit the proxy setting
# https://support.rstudio.com/hc/en-us/articles/200488488-Configuring-R-to-Use-an-HTTP-or-HTTPS-Proxy

# define function to convert `Array of objects` into dataframe #################
json_df <- function(l) {
    
    library(qdap)
    metric <- list2df( l$metric, col1 = "metric")
    value <- list2df( l$dps, col1 = 'value', col2 = 'timestamp')
    ip <- list2df( l$tags, col1 = 'ip', col2 = 'iptype')
    
    v <- value %>%
        mutate(metric = metric$metric %>% as.character(),
               metric = if_else(metric == 'netq.recv.601', 'perc_receive', 'delay'),
               timestamp = as.integer(timestamp),
               sip = ip %>% filter(iptype == 'sip') %>% select(ip) %>% as.character(),
               dip = ip %>% filter(iptype == 'dip') %>% select(ip) %>% as.character(),
               datetime = as.POSIXct(timestamp, origin = "1970-01-01")
        )
    
    return(v)
    
}

# read file into ###############################################################
json_file <- list.files(path = './rawData')

rawdata <- data.frame()
for (i in 1:length(json_file)) {
    
    js_file <- paste0('./rawData/', json_file[i])
    js_data <- fromJSON(paste(readLines(js_file, warn = F), collapse = ""),
                        simplifyVector = FALSE, simplifyDataFrame = FALSE, flatten = TRUE)
    
    jdtf <- lapply(js_data$data, json_df)
    dtf <-  bind_rows(jdtf) %>%
        spread(metric, value) %>%
        tbl_df()
    
    rawdata <- rbind.data.frame(rawdata, dtf)
    
    write.csv(x = dtf, file = paste0("./rawDataCSV/",json_file[i],'.csv', na = "NULL"), row.names = FALSE)
    write.csv(x = ip_file, file = paste0('./uniqueIP/',json_file[i],'_IP.csv', na = "NULL"), row.names = FALSE)
    
    print(paste0('success: ',json_file[i]))
}

## build dataset to store all of netq - aws raw data ###########################
write.csv(x = rawdata, file = 'rawDataSet.csv', row.names = FALSE, na = "")

# http://ip-api.com/#123.151.176.141
sip <- unique(rawdata$sip)
sip_location <- ip_api(ip_addresses = sip,  as_data_frame = TRUE, delay = 150 < length(sip)) %>%
    mutate(ip = sip, type = 'sip')

dip <- unique(rawdata$dip)
dip_location <- ip_api(ip_addresses = dip,  as_data_frame = TRUE, delay = 150 < length(dip)) %>%
    mutate(ip = dip,  type = 'dip') 

# build ip location mapping to find country, ISP, CITY #########################
ip_location <- rbind(sip_location, dip_location) %>%
    select(-country_code, -region_code, -zip_code)

write.csv(x = ip_location, file = "ip_location.csv", row.names = FALSE, na = "NULL")
write.xlsx(x =ip_location, file = "ip_location.xlsx")


# have to set the literal "NULL" as NULL
# SELECT *
#    FROM DW_WOF_POSTLAUNCH.[user].fhcnNetwork
# WHERE perc_receive = 'NULL' AND [delay] = 'NULL'


# UPDATE DW_WOF_POSTLAUNCH.[user].fhcnNetwork
# SET [delay] = NULL
# WHERE [delay] = 'NULL'


### build path on map
sip_dip <- rawdata %>% 
    tbl_df() %>%
    select(sip, dip) %>%
    unique() %>%
    mutate(lpath = paste0(sip, '->', dip))

t <- sip_dip %>%
    gather()

