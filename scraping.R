dataScraping <- function(lien,limInf = as.numeric(0), limSup = as.numeric(0)){

genericLink <- paste(lien, sep="")
#we initialize the data frame that will gather all of the results from every pages of the website 
Data <- NULL

#we need to loop over the entire website (22 pages of results)
for (p in as.numeric(limInf):as.numeric(limSup)){
    
    link <- paste(genericLink,p, sep="")
    con <- url(link,'r')
    web_page <- readLines(con)
    
    #we need to revamp part of the data as some data are missing as they were not provided by the atletes
    web_page1 <- gsub("></",">NA</", web_page)
    test <- regexpr("<td align=\"right\" colspan=\"1\">(.*?)</td><td colspan=\"1\"><i>(.*?)</i>(.*?)</td><td align=\"center\" colspan=\"1\">(.*?)</td><td class=\" bold\" colspan=\"1\"><b>(.*?)</b>(.*?)</td><td align=\"center\" colspan=\"1\">(.*?)</td><td align=\"center\" colspan=\"1\">(.*?)</td><td colspan=\"1\">(.*?)</td><td align=\"right\" colspan=\"1\">(.*?)</td><td class=\" bold\" align=\"right\" colspan=\"1\"><b>(.*?)</b>(.*?)</td><td colspan=\"1\">(.*?)</td><td colspan=\"1\"><i>(.*?)</i>(.*?)</td><td align=\"right\" colspan=\"1\">(.*?)</td><td colspan=\"1\">(.*?)</td><td colspan=\"1\"><i>(.*?)</i>(.*?)</td>", web_page1)  
    m <-regmatches(web_page1, test)
    main_table <- gsub("<table cellspacing=(.*?)cellpadding=(.*?)style=(.*?)>|</table>|<tr(.*?)>|<td(.*?)>|</td>|</tr>|<img(.*?)>|<div(.*?)>|</div>{1}|<i>|</i>|<b>|</b>", "§", m)
    
    sub_main_table <- gsub("§+", "§", main_table)
    table <- strsplit(sub_main_table, "§")
    
    #let's do a dataframe out of this table
    DF <- NULL
    for (l in 1:length(table)){
      #Attention, si on ne fait pas un petite manipulation DF a la structure d'une liste..
      a <- table[l]
      DF <- rbind(DF, a[[1]])
    }
    DF <- data.frame(DF)
    
    #we were conservative we now need to delete some columns
    drops <- c("X1", "X4", "X7", "X13", "X16", "X20")
    DF <- DF[,!names(DF) %in% drops]
    
    #Let's store this data
    #fileName <- paste("table_",sprintf("%03i.csv",as.numeric(p)), sep="")
    #write(computationDF, file = fileName,
    #      ncolumns = if(is.character(DF)) 1 else 14,
    #      append = FALSE, sep = ",")
    
    #we can then concatenate with the rbind function every page of the website
    Data <- rbind(Data, DF)
    closeAllConnections() 
  }
  
  #we can rename the title of each column
  cat_names <- c("Position", "Number", "Sex", "Name", "Age", "Country", "Team", "Real_Time", "Time", "TKM", "Avg", "Cat_rank", "Cat_name","Localization")
  names(Data) <- cat_names
  
  Data
  
}
