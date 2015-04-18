library(RCurl)
library(XML)
library(data.table)
library(ggplot2)

geomSeries <- function(base, max) {
  base^(0:floor(log(max, base)))
}

asinh_trans <- function()
{
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

draw <- function(df, acc = unique(df$account), 
                 year = "2014", type = "balance", class = NULL)
{
  class_name <- NULL
  if (is.null(class) == FALSE)
  {
    acc <- acc[ as.character(class) == sapply(acc, substr, start = 1, stop = 1)]
    class_name <- c("Необоротні активи", "Запаси", "Кошти, розрахунки та інші активи",
        "Власний капітал", "Довгострокові зобов'язання", "Поточні зобов'язання", 
        "Доходи", "Витрати")[class]
    
  }
  if (type == "balance")
  {
    df <- df[!(as.character(df$account) %in% c("00","01","02","03","04","05","06","07","08","09")),]
  }
  exclude_solo <- function(d)
  {
    acc <- unique(d$account)
    ex <- character(0)
    for (i in 1:length(acc))
    {
      ##      print(paste(acc[i], sum(d$account == acc[i])))
      if (sum(d$account == acc[i])==1)
      {
        ex <- c(ex, as.character(acc[i]))
      }
    }
    d[!(d$account %in% ex),]
  }
  add_temp_points <- function(d)
  {
    d2 <- rbind(
      d,
      transform(d[order(d$dt),],
                dt=dt - 1e-9, 
                saldo=ave(saldo, account, FUN=function(z) c(z[[1]], head(z, -1L)))
      )) 
    #     for (i in 1:(length(d$dt)-1))
    #     {
    #       if (d$account[i] == d$account[i+1]) 
    #       {
    #         addn <- data.frame(cbind(d$account[i],d$dt[i+1],d$saldo[i]))
    #         names(addn) <- names(d2)
    #         addn$account <- d$account[i+1]
    #         addn$dt <- d$dt[i+1]
    #         addn$saldo <- d$saldo[i]
    #         d2 <- rbind(d2, addn)
    #       }
    #     }
    d2
  }
  firstDay <- paste("0101",year, sep = "")
  lastDay <- paste("3112",year, sep = "")
  fDay <- as.Date(firstDay, "%d%m%Y"); 
  lDay <- as.Date(lastDay, "%d%m%Y"); 
  df <- df[df$account %in% acc,]
  df <- df[(df$dt >= fDay) & (df$dt <= lDay),]
  df <- exclude_solo(df)
  df$account <- factor(df$account)
  #   df <- add_temp_points(df)
  df$saldo <- df$saldo / 1000
  #   b <- geomSeries(base = 10, max = 10000)
  #   br <- c(rev(b)*(-1),0,b)
  #   ggplot(df, aes(x=dt, y=saldo, fill=account)) + geom_area() + 
  #                 xlab("Період") + ylab("Сальдо, тис. грн") 
  #  ggplot(data=df, aes(x=dt, y=saldo, group=account, colour=account)) + geom_line()  + 
  #   
  gr <- ggplot(data=df, aes(x=dt, y=saldo, colour=account)) + geom_step()  +
    xlab("Період") + ylab("Сальдо, тис. грн") 
  if (is.null(class_name) == FALSE)
  {
    gr <- gr + ggtitle(class_name)
  }
  gr
  #  gr + scale_y_continuous(trans = 'asinh', breaks = br)
  #  ggsave(file = "plot.jpg", dpi = 2000)
}

data_download <- function ()
{   
  buch = NULL
  fileURL="http://www.mon.gov.ua/files/1c/D05122014.TXT"
  nams <- read.csv(file = fileURL, header = TRUE, 
                   sep = "\t", row.names=NULL, fileEncoding = "cp1251", dec = ",", nrow = 1);
  nams2 <- read.csv(file = fileURL, header = TRUE, 
                    sep = "\t", row.names=NULL, fileEncoding = "cp1251", skip = 1, dec = ",", nrow = 1);
  a <- names(nams)
  b <- names(nams2)
  a[grep("X",a)] <- b[grep("X",a)]
  xm <- htmlTreeParse("http://www.mon.gov.ua/ua/public_information/vidkr_buch/", 
                      useInternal = TRUE)
  fileUrls <- xpathSApply(xm, "//td/a",xmlAttrs)
  fileUrls <- fileUrls[grep("TXT", fileUrls, ignore.case = TRUE)]
  fileUrls <- fileUrls[!(fileUrls %in% fileUrls[grep("Q", fileUrls, ignore.case = TRUE)])]
  for (k in length(fileUrls):1){
    print(fileUrls[k]);
    buch <- rbind(buch, read.csv(file = paste("http://www.mon.gov.ua",fileUrls[k], sep = ""), header = T, 
                                   sep = "\t", row.names=NULL, fileEncoding = "cp1251", skip = 1, dec = ","));
    Sys.sleep(4)
  }
  names(buch) <- a
  buch$ДЕБЕТ[is.na(buch$ДЕБЕТ)] <- "0"
  buch
}

accounts_dates <- function (buch)
{
  rem_desc <- function(s)
  {
    s1 <- substr(s,1,3)
    if (substr(s,3,3) == ":")
    {
      s1 <- substr(s1,1,2)
    }
    s1
  }
  buch <- buch[!is.na(buch$Сума), ]
  buch$ДЕБЕТ[is.na(buch$ДЕБЕТ)] <- "0"
  buch$ДЕБЕТ <- sapply(buch$ДЕБЕТ, rem_desc)
  buch$КРЕДИТ <- sapply(buch$КРЕДИТ, rem_desc)  
  acc <- unique(c(buch$ДЕБЕТ[!is.na(buch$ДЕБЕТ)], buch$КРЕДИТ))
  rem <- c("0",":")
  acc <- acc[!acc %in% rem]
  
  buch$Дата.операції <- as.Date(strptime(as.character(buch$Дата.операції), 
                                         "%d.%m.%Y %H:%M:%S"))
  count_saldo <- function(d, acc = NULL)
  {
    
    sum_deb <- sum(buch$Сума[(buch$ДЕБЕТ == acc) &  (buch$Дата.операції <= d)])
    sum_cred <- sum(buch$Сума[(buch$КРЕДИТ == acc) &  (buch$Дата.операції <= d)])
    if (is.na(sum_deb - sum_cred)) 
    {
      print("Лови момент!")
    }
    as.numeric(sum_deb - sum_cred)
    
  }
  account <- numeric(0)
  dt <- structure(numeric(0), class = "Date")  #as.Date(numeric(0), format = "%d.%m.%Y %H.%M.%S")
  saldo <- numeric(0)
  for (i in 1:length(acc))
  {
    d <- unique(buch$Дата.операції[(buch$ДЕБЕТ == acc[i]) | (buch$КРЕДИТ == acc[i])])
    dt <- c(dt, d)
    saldo <- c(saldo, sapply(d, count_saldo, acc[i]))
    account <- c(account, rep(acc[i],length(d)))
  }
  ret <- data.frame(cbind(account,dt,saldo))
  ret$dt <- dt
  ret$saldo <- saldo
  ret
}

load_data <- function(fl = "buch.Rda")
{
  load("buch.Rda", .GlobalEnv)
}
