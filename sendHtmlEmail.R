library(xlsx)
library(dplyr)
library(stringr)
library(htmlTable)
library(sendmailR)

today <- Sys.Date()

drive <- "//xxx/Private/JIRADashboard/"
projects <- c("PlayLink","TrivialPursuit")
mail_tag <- c("[PL_UNO]", "[TP_NX]")
# "xxxx@xxx.com", "xxxx@xxx.com"
recipient <- c("xxxx@xxx.com", "xxxx@xxx.com")

from <- "xxxx@xxx.com"
to <- recipient
bcc <- "xxxx@xxx.com"
mailControl = list(smtpServer = "smtp-xxx.xxxx.org")

col.log <- "#E2EFD9" # color for group logged time
col.not.log <- "#FFF2CC" # color for group didn't log time

for (i in projects) {
    
    # calculate the result to present members who didn't log time
    f <- try({
        path <- paste0(drive, i)
        file_df <- data.frame(file = list.files(path), stringsAsFactors = F) %>%
            mutate(date = str_extract(file,"\\d+\\.\\d+\\.\\d+"),
                   date = as.Date(date, format = "%Y.%m.%d"),
                   log = str_extract(file,"\\d+\\.\\d+\\.\\d+"),
                   file_path = paste0(path,"/",file)
            ) %>%
            filter(!is.na(date)) %>%
            arrange(desc(date)) %>%
            top_n(n = 2, wt = date)
        
        file_df$file_name <- c("l", "e")
        
        # l <- read.xlsx(file = file_df$file_path[1], stringsAsFactors = FALSE, sheetIndex = 1)
        # e <- read.xlsx(file = file_df$file_path[2], stringsAsFactors = FALSE, sheetIndex = 1)
        
        for (f in file_df$file_path) {
            temp <- read.xlsx(file = f, stringsAsFactors = FALSE, sheetIndex = 1) %>%
                distinct() %>% # remove duplicate rows
                mutate(sptNumber = as.numeric(str_extract(Sprint, "\\d+")))
            
            names(temp)[3] <- "Assignee"
            file_name <- file_df %>% filter(file_path == f) %>% select(file_name) %>% as.character()
            assign(x = file_name, value = temp)
            
            max_spt_name <- paste0('max_spt_', file_name)
            assign(x = max_spt_name, value = max(temp$sptNumber, na.rm = T))
            
        }
        
        # find the MAX sprint
        # solve the problem when new sprint created
        max_spt <- list.files(path, pattern = '.txt') %>% 
            str_extract('\\d+') %>% 
            as.numeric() %>%
            min(max_spt_l, max_spt_e)
        
        l %>%
            filter(sptNumber == max_spt) %>%
            group_by(Assignee) %>%
            summarise(timeSpent_l = round(sum(Time.Spent, na.rm = T), 1)) %>%
            ungroup() %>%
            left_join(e %>%
                          filter(sptNumber == max_spt) %>%
                          group_by(Assignee) %>%
                          summarise(timeSpent_e = round(sum(Time.Spent, na.rm = T), 1)) %>%
                          ungroup(), 
                      by = "Assignee") %>%
            mutate(timeSpent_e = if_else(is.na(timeSpent_e), 0, timeSpent_e),
                   If_logged = if_else(timeSpent_e < timeSpent_l, "Logged", "Didn't Log"),
                   timeLoged = timeSpent_l- timeSpent_e,
                   col = if_else(If_logged == "Didn't Log", col.not.log, col.log)) %>%
            arrange(If_logged, Assignee) %>%
            select(Assignee,`YesterdayTimeLogged(H)` = timeLoged, If_logged, col)
    })
    
    
    if (class(f)[1] != 'try-error') {
        
        f <- as.data.frame(f)
        row.names(f) <- f$Assignee
        
        # transform tables into HTML formatting
        nb_groups <- f %>%
            group_by(If_logged, col) %>%
            summarise(n = n())
        y <- htmlTable(f %>% select(2),   
                       rgroup   = nb_groups$If_logged,
                       n.rgroup = nb_groups$n,
                       col.rgroup = nb_groups$col,
                       tfoot = "&dagger; Note, if you take leaves or did not implement the project JIRA tasks yesterday, the log time might be 0",
                       css.table = "margin-top: 0.2em; margin-bottom: 0.2em; width: 50%;")
        
        # send mail
        body <- paste0("<html>", y,
                       "</html>")
        id <- which(projects == i)
        subject <- paste0(mail_tag[id]," ",
                          nb_groups$n[1], '/',
                          sum(nb_groups$n),
                          " members didn't log time by ",
                          file_df$log[1],
                          " within Sprint ",
                          max_spt)
        to_ <- to[id]
        msg <- mime_part(body)
        msg[["headers"]][["Content-Type"]] <- "text/html"
        
        print(paste0(i, " successful"))
    } else {
        
        body <- paste0("<html>", 'There is error with file', "</html>")
        subject <- paste0('Error ', ' within Sprint ', max_spt)
        to_ = 'jing.wang@ubisoft.com'
        msg <- mime_part(body)
        msg[["headers"]][["Content-Type"]] <- "text/html"
        
    }
    
    sendmail(from=from, to=to_, bcc = bcc, subject=subject, msg=msg, control=mailControl)
    
}
