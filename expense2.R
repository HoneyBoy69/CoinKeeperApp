library(data.table)
library(zoo)
library(RColorBrewer)

#read file
export <- read.csv("export.csv", sep = ";")

#extract only income + expenses
DT <- data.table(export[1:5496,])

#delete columns
col.del <- colnames(DT)[c(6:7,10)]
DT[, (col.del) := NULL]

#rename column
setnames(DT,"Amount.converted","Amount")
setnames(DT,"Currency.of.conversion","Currency")

#convert all columns to character
DT <- DT[,lapply(DT, as.character)]

#convert Data to Date format
DT$Data <- gsub("/", ".", DT$Data)
DT[, Data := as.Date(Data, format='%m. %d. %Y')]

#convert Amount to numeric
DT$Amount <- gsub(",", ".", DT$Amount)
DT <- DT[-c(78), ]
DT[, Amount := as.numeric(Amount)]

#replace empty value in Tags with "not set"
DT$Tags[which(DT[, Tags == ""])] <- "not set"

#create column with month + year
DT$MonY <- as.yearmon(DT[, Data])
#create column with month
DT$Month <- as.Date(cut(DT$Data, breaks = "month"))

#subset only expenses
df_expenses <- DT[Type == "Expense"]

#subset only income
df_income <- DT[From %in% c("Income", "Belyash Work", "Alisyusha Work") & To != "Sber deposit"]

#subset only transfer
s <- c("Fund X", "USD Fund", "EUR fund", "Metal Fund", "Deposit VTB", "BTC Depo", "USD Tinkoff", "USD Tinkoff")
df_fund <- DT[(To %in% s) | (From %in% s)]
  
#rename tags
#Shopping
df_expenses[To == "Shopping"]$Tags[which(df_expenses[To == "Shopping"][, Tags == "MTG"])] <- "Board Games"
df_expenses[To == "Shopping"]$Tags[which(df_expenses[To == "Shopping"][, Tags == "Delivery"])] <- "Other"

#Eating outside
df_expenses[To == "Eating outside"]$Tags[which(df_expenses[To == "Eating outside"][, Tags == "Fast food"])] <- "Take out"
df_expenses[To == "Eating outside"]$Tags[which(df_expenses[To == "Eating outside"][, Tags == "Bakery"])] <- "Take out"
df_expenses[To == "Eating outside"]$Tags[which(df_expenses[To == "Eating outside"][, Tags == "not set"])] <- "Take out"

#Transport
df_expenses[To == "Transport"]$Tags[which(df_expenses[To == "Transport"][, Tags == "Metro"])] <- "Podorozhnik"
df_expenses[To == "Transport"]$Tags[which(df_expenses[To == "Transport"][, Tags == "not set"])] <- "Podorozhnik"
df_expenses[To == "Transport"]$Tags[which(df_expenses[To == "Transport"][, Tags == "Surface"])] <- "Podorozhnik"

#Pets
df_expenses[To == "Pets"]$Tags[which(df_expenses[To == "Pets"][, Tags == "not set" & MonY == "мар 2015"])] <- "Vet"
df_expenses[To == "Pets"]$Tags[which(df_expenses[To == "Pets"][, Tags == "not set" & MonY == "май 2015"])] <- "Litter"
df_expenses[To == "Pets"]$Tags[which(df_expenses[To == "Pets"][, Tags == "not set" & MonY == "июн 2015"])] <- "Food"

#Groceries
df_expenses[To == "Groceries"]$To[which(df_expenses[To == "Groceries"][, Tags == "Alcohol"])] <- "Entertainment"
df_expenses[To == "Groceries"]$Tags[which(df_expenses[To == "Groceries"][, Tags == "Fruits and veggies, Candies"])] <- "Fruits and veggies"
df_expenses[To == "Groceries"]$Tags[which(df_expenses[To == "Groceries"][, Tags == "Drinks, Cigarettes"])] <- "Cigarettes"
df_expenses[To == "Groceries"]$Tags[which(df_expenses[To == "Groceries"][, Tags == "Poultry"])] <- "Meat"
df_expenses[To == "Groceries"]$Tags[which(df_expenses[To == "Groceries"][, Tags == "Pizza"])] <- "Premade"

#Entertainment
df_expenses[To == "Entertainment"]$Tags[which(df_expenses[To == "Entertainment"][, Tags == "Condoms"])] <- "Ero"
df_expenses[To == "Entertainment"]$Tags[which(df_expenses[To == "Entertainment"][, Tags == "Museums & Parks"])] <- "Museums & Cinema"
df_expenses[To == "Entertainment"]$Tags[which(df_expenses[To == "Entertainment"][, Tags == "Cinema"])] <- "Museums & Cinema"
df_expenses[To == "Entertainment"]$To[which(df_expenses[To == "Entertainment"][, Tags == "Marvel"])] <- "Services"

#Services
df_expenses[To == "Services"]$Tags[which(df_expenses[To == "Services"][, Tags == "not set"])] <- "Other"
df_expenses[To == "Services"]$Tags[which(df_expenses[To == "Services"][, Tags == "Internets"])] <- "Other"
df_expenses[To == "Services"]$Tags[which(df_expenses[To == "Services"][, Tags == "Security"])] <- "Other"
df_expenses[To == "Services"]$Tags[which(df_expenses[To == "Services"][, Tags == "Skype"])] <- "App"
df_expenses[To == "Services"]$Tags[which(df_expenses[To == "Services"][, Tags == "Marvel"])] <- "App"
df_expenses[To == "Services"]$Tags[which(df_expenses[To == "Services"][, Tags == "Realty"])] <- "Relocation"
df_expenses[To == "Services"]$Tags[which(df_expenses[To == "Services"][, Tags == "Realty"])] <- "Relocation"
df_expenses[Tags == "Hairdresser"]$To <- "Beauty"
df_expenses[To == "Services"]$To[which(df_expenses[To == "Services"][, Tags == "Travel Services"])] <- "Travel"

#Health
df_expenses[To == "Health"]$To[which(df_expenses[To == "Health"][, Tags == "Beauty"])] <- "Beauty"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "авг 2016"])] <- "Dental"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "июн 2016"])] <- "Doc"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "сен 2015"])] <- "Doc"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "июн 2015"])] <- "Doc"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "май 2015"])] <- "Doc"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "апр 2015"])] <- "Doc"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "авг 2015"])] <- "Meds"
df_expenses[Tags == "Health"]$Tags[which(df_expenses[Tags == "Health"][, MonY == "сен 2016"])] <- "Meds"
df_expenses[Tags == "Health"]$Tags <- "Optics"

#Beauty
df_expenses[To == "Beauty"]$Tags[which(df_expenses[To == "Beauty"][, Tags == "not set"])] <- "Skin Care"
df_expenses[To == "Beauty"]$Tags[which(df_expenses[To == "Beauty"][, Tags == "Cleaning tools"])] <- "Skin Care"
df_expenses[Tags == "Beauty"]$Tags[which(df_expenses[Tags == "Beauty"][, MonY == "мар 2015"])] <- "Hairdresser"
df_expenses[Tags == "Beauty"]$Tags[which(df_expenses[Tags == "Beauty"][, MonY == "апр 2015"])] <- "Hairdresser"
df_expenses[Tags == "Beauty"]$Tags[which(df_expenses[Tags == "Beauty"][, MonY == "фев 2015"])] <- "Beauty Master"
df_expenses[Tags == "Beauty"]$Tags[which(df_expenses[Tags == "Beauty"][, MonY == "окт 2015"])] <- "Beauty Master"
df_expenses[Tags == "Beauty"]$Tags[which(df_expenses[Tags == "Beauty"][, MonY == "май 2015"])] <- "Bath"
df_expenses[Tags == "Beauty"]$Tags[which(df_expenses[Tags == "Beauty"][, MonY == "сен 2015"])] <- "Skin Care" 
df_expenses[Tags == "Beauty"]$Tags <- "Cosmetics"  

#House
df_expenses[To == "House"]$Tags[which(df_expenses[To == "House"][, Tags == "Chemicals"])] <- "Cleaning tools"

#Travel
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"][, Tags == "Hotel"])] <- "SPB 04/15, Travel Rent"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"][, Tags == "Travel Transport, Moscow"])] <- "Moscow 11/15, Travel Transport"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"][, Tags == "Moscow"])] <- "Moscow 03/15, Travel Transport"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"][, Tags == "Izhevsk"])] <- "Izhevsk 08/15, Travel Eating"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"][, Tags == "Taxi, Travel Eating"])] <- "USA 10-11/15, Travel Eating"

df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"]
                                  [,Tags == "Travel Shopping" & (MonY == "ноя 2015" | MonY == "окт 2015")])] <- "USA 10-11/15, Travel Shopping"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"]
                                       [,Tags == "Travel Transport" & (MonY == "ноя 2015" | MonY == "окт 2015")])] <- "USA 10-11/15, Travel Transport"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"]
                                       [,Tags == "Travel Groceries" & (MonY == "ноя 2015" | MonY == "окт 2015")])] <- "USA 10-11/15, Travel Groceries"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"]
                                       [,Tags == "Travel Eating" & (MonY == "ноя 2015" | MonY == "окт 2015")])] <- "USA 10-11/15, Travel Eating"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"]
                                       [,Tags == "Travel Rent" & (MonY == "ноя 2015" | MonY == "дек 2015")])] <- "LenOb 12/15, Travel Rent"
df_expenses[To == "Travel"]$Tags[which(df_expenses[To == "Travel"]
                                       [,Tags == "Travel Entertainment" & 
                                         (MonY == "ноя 2015" | MonY == "окт 2015")])] <- "USA 10-11/15, Travel Entertainment"


#add color for category
df_expenses$color <- suppressWarnings(factor(df_expenses$To, labels = colorRampPalette(RColorBrewer::brewer.pal(12, name = 'Spectral'))(13)))

#add color for subcategory
df_expenses$sub_color <- rep("0", nrow(df_expenses))
for (i in unique(df_expenses$To)) { 
  t <-  factor(df_expenses[To == i]$Tags, 
               labels = colorRampPalette(RColorBrewer::brewer.pal(11, name = 'Spectral'))
               (length(unique(df_expenses[To == i]$Tags))))
  df_expenses[To == i, sub_color := t]
  }

#change yellow color
df_expenses[color == '#FFFFBF']$color <- "#946DD6"
df_expenses[sub_color == '#FFFFBF']$sub_color <- "#946DD6"

df_expenses[color == '#EAF69E']$color <- "#6DB0D6"
df_expenses[sub_color == '#EAF69E']$sub_color <- "#6DB0D6"

#month for select input in UI
period <- unique(df_expenses$MonY)

#find currencies
sberometer <- readLines("https://www.sberometer.ru/cbr/")
usd_rub = sberometer[763]
eur_rub = sberometer[766]

#currencies convertor
conv_rub = function(x) {
  x = gsub("<[^<>]+>", "", x)
  x = gsub("[^0-9\\.0-9]", "", x)
  as.numeric(x)
}

#table for capital accumulation
m <- NULL
n <- NULL
k <- NULL 
l <-  0
usd = conv_rub(usd_rub)
eur = conv_rub(eur_rub)
  
#loop through df_fund, convert currency RUB, determine income or expense
for (i in 1:nrow(df_fund)) {
  if ((df_fund$To[i] %in% s) & !(df_fund$From[i] %in% s)) {
    if (df_fund$Currency[i] == 'USD') {
      usd_con <- df_fund$Amount[i] * usd
      m = append(m, df_fund$Month[i])
      k = append(k, df_fund$MonY[i])
      n = append(n, usd_con)
    } else if (df_fund$Currency[i] == 'EUR') {
      eur_con <- df_fund$Amount[i] * eur
      m = append(m, df_fund$Month[i])
      k = append(k, df_fund$MonY[i])
      n = append(n, eur_con)
    } else {
      m = append(m, df_fund$Month[i])
      k = append(k, df_fund$MonY[i])
      n = append(n, df_fund$Amount[i])
    }
    
  } 
  else if ((df_fund$From[i] %in% s) & !(df_fund$To[i] %in% s)) {
    if (df_fund$Currency[i] == 'USD') {
      usd_con <- df_fund$Amount[i] * usd
      m = append(m, df_fund$Month[i])
      k = append(k, df_fund$MonY[i])
      n = append(n, usd_con *(-1))
    } else if (df_fund$Currency[i] == 'EUR') {
      eur_con <- df_fund$Amount[i] * eur
      m = append(m, df_fund$Month[i])
      k = append(k, df_fund$MonY[i])
      n = append(n, eur_con *(-1))
    } else {
      m = append(m, df_fund$Month[i])
      k = append(k, df_fund$MonY[i])
      n = append(n, df_fund$Amount[i] *(-1))
    }
  }
}

#create data.table and group by Month
df_accum <- data.table("Month" = m, "MonY" = as.yearmon(k), "Transfer" = n)
df_accum <- df_accum[, .(Transfer = sum(Transfer)), by = .(Month, MonY)]
df_accum$Accum <- rep(0, nrow(df_accum))

#create column with capital accumulation
for (i in 1:nrow(df_accum)) {
  l = l + df_accum$Transfer[i]
  df_accum$Accum[i] = round(l, 1)
}

df_accum <- rbind(df_accum, data.table(Month = as.Date('2015-02-01'), MonY = as.yearmon('фев 2015'), Transfer = 0, Accum = 0))
df_accum <- df_accum[order(Month)]



x <- df_expenses[To == "Travel"]
foo <- data.frame(do.call('rbind', strsplit(as.character(x$Tags),',',fixed=TRUE)))