library(plyr)
library(ggplot2)
setwd("C:/Users/Frank/Desktop/Spending Analysis")

# Read in the statements
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)

# Ordering the transactions
AllData <- ldply(paths, read.csv)
AllData$Posted.Date <- as.Date(AllData$Posted.Date, "%m/%d/%Y")
AllData$Payee <- as.character(AllData$Payee)
AllData <- AllData[order(AllData$Posted.Date, decreasing = TRUE),]
AllDataF <- subset(AllData, select = -c(.id, Reference.Number))

# Adding class field
AllDataF$class <- "Other"

# regexp strings
COFFEE <- "STARBUCKS|LOLLICUP COFFEE|LOLLICUP|KEAN COFFEE"
GROCERYGAS <- "RALEY'S|WM|WAL-MART|TRADER JOE'S|99 RANCH|SAMS CLUB|SAMSCLUB|CHEVRON|WATT CIRCLE SEVEN|VALERO"
FOOD <- "SUBWAY|RAMEN & RICE|FAHRENHEIT|WENDY'S|MCDONALD'S|THAI HOUSE|TERIYAKI GRILL|PINKBERRY|DON QUIXOTES MEXICAN|SHABU JAPANESE|LUNCHSTOP AT&T WATT|BURGER KING|IN-N-OUT|SKIPS KITCHEN|HENRY'S HUNAN|CHANDO'S TACOS|OSCAR'S VERY MEXICAN|BENTO BOX|APPLEBEES|IHOP|ARIGATO|CHIPOTLE|DEL TACO|KATHMANDU KITCHEN|WOODYS|HONEY DONUTS|YUMMY CAFE|MIRAGE 4 U|FAMOUS PIZZA|SUSHI HOOK|ALIOTO'S WATERSIDE CAF|CURRY VILLAGE FOOD&BEV|EASTERN EMPIRE|HOULIHANS RESTAURANT|JACK'S URBAN EATS|LITTLE SAIGON|POPEYE'S|TAJ BAR & GRILL|ARAMARK|BJS RESTAURANTS|PANERA BREAD|MEHFIL INDIAN|SHAHRZAD FINE CUISI|WAFFLE SHOP|LA TORTILLA|RAMEN & MORE|SIZZLER REST|BOKA|ESS-A-BAGEL|POURING RIBBONS|SARANGBANG|THE HABIT|FAHRENHEIT 250 BBQ|CORNER STORE|BAGEL STREET|PISTO'S TACOS|RUBIO'S|PANDA EXPRESS|PHO BAC HOA VIET|SHOKI RAMEN|SAVOURIE STREETS|ARMADILLO WILLYS BBQ|LA VICTORIA|RUSSIAN RIVER BREWING|SCOTTY'S|CHINA BUFFET|DENNY'S|YOSEMITE LDG FOOD&BEV|BACHI BURGER LAS VEGAS NV|CAFFE DEL MONDO SAN FRANCISCOCA| COSMOPOLITAN BUFFET WS LAS VEGAS NV| GRAND CANYON RESTAUR WILLIAMS AZ|HASH HOUSE A GO GO LAS VEGAS NV|JACK IN THE BOX|JAMBA JUICE #474|JOHNNY ROCKETS - 401 LAS VEGAS NV|MGM CASINO BAR LAS VEGAS NV|SAFEWAY|SW STEAKHOUSE REST/BAR|WOWPOINTSCOM*RESTAURAN|CHEWYS|TAQUERIA|KFC/AW|SUSHI AI LONG BEACH CA|NATIVE FOODS|EDWARDS ALHAMBRA RENAI|SIMMZY'S LONG BEACH|PORTOS BAKERY DOWNEQPS|JEO JAC KEO RI HAWAIIAN|PETERS GOURMADE GRILL|THE BREAD BASKET|YOGURTLAND|PHAYATHAI|COSMOPOLITAN BUFFET|DADS KITCHEN|GRAND CANYON RESTAUR|REMEN & MORE|SAMUEL HORNE'S|SAIGON RESTAURANT|SAMS SUB SHOP|BATTLE AND BREW SANDY SPRINGSGA|32COMPANYKITCHEN 877-2 MERRIAM KS|JIMMY JOHNS 986 ATLANTA GA|THE CLUB HOUSE CARMICHAEL CA|NUGGET MARKET 08 ELK GROVE|BRENDA'S MEAT & TH SAN FRANCISCOCA|BRENDEN 14 AT THE PALM LAS VEGAS NV|CHICAGO FIRE ROSEVILLE ROSEVILLE CA|HUONG LAN SANDWICHES 3 SACRAMENTO CA|GRUBYS NEW YORK DE ATLANTA GA|MUSCLE MAKER GRILL - S SAN RAMON CA|PHO DAI LOI 2 ATLANTA GA"
MOVIES <- "CENTURY THEATRES|REDBOX|REGAL CINEMAS|PALLADIO CINEMAS"
CAR <- "MAITA SUBARU|QUICK QUACK CARWASH|PAVILION CAR CARE CENT"
AIRLINETRAINHOTEL <- "AMTRAK|HOTWIRE-SALES|UNITED|BART|VIR AMER|LYONS|NJT NY PENN STA|NJT SUMMIT|CALTRAIN|FOUR PTS BY SHERATON|JETBLUE|SOUTHWES|DELTA.COM"
MEDICAL <- "MUSBAH AL-SALTI MD|IN *CAPITAL DERMATOLOG|DENTAL SOURCE|LCA*LABCORP"

# Apply regexp and return their class
AllDataF$class <-
  ifelse(grepl(COFFEE, AllDataF$Payee), "COFFEE",
         ifelse(grepl(GROCERYGAS, AllDataF$Payee), "GROCERY/GAS",
                ifelse(grepl(FOOD, AllDataF$Payee), "FOOD",
                       ifelse(grepl(MOVIES, AllDataF$Payee), "MOVIES",
                              ifelse(grepl(CAR, AllDataF$Payee), "CAR",
                                     ifelse(grepl(AIRLINETRAINHOTEL, AllDataF$Payee), "AIRLINE/TRAIN/HOTEL",
                                            ifelse(grepl(MEDICAL, AllDataF$Payee), "MEDICAL", "Other")))))))

## Graphing

# Month aggregation
AllDataF$Month <- cut(AllDataF$Posted.Date, breaks="month")

# Remove all payments
AllDataF <- subset(AllDataF, AllDataF$Amount < 0)

# Build summary table of monthly spend per class
summary <- ddply(AllDataF, .(Month, class), summarise, cost=abs(sum(Amount)))

# Time series plot
ggplot(summary, aes(Month, cost, col=class, group=1)) +
  facet_wrap(~class, ncol=2, scale="free_y") +
  geom_smooth(method="loess", se=F) + geom_point() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.position="none") +
  labs(x="", y="Monthly total ($)")

# # Calculating average expenses
# AvgExpense <- ddply(summary, .(class), summarise, m=mean(cost))
# ggplot(AvgExpense, aes(x=class, y=m)) +
#   geom_bar(stat="identity") +
#   labs(y="Average Monthly Expense ($)", x="")

# # Checking the most frequent Other
DFOther <- subset(AllDataF, AllDataF$class == "Other")
sort(table(unlist(lapply(DFOther$Payee, paste, collapse = " "))), decreasing=TRUE)

# Finding Monthly Expenditure
monthlySum <- ddply(AllDataF, .(Month), summarise, cost=abs(sum(Amount)))
monthlySum <- monthlySum[-nrow(monthlySum),]
ggplot(monthlySum, aes(Month, cost, group=1)) +
  geom_smooth(method="loess", se=F) + geom_point() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.position="none") +
  labs(x="", y="Monthly total ($)")

# Everything besides airline/train/hotel
NOAIRAllDataF <- subset(AllDataF, AllDataF$class != "AIRLINE/TRAIN/HOTEL")

NOAIRSum <- ddply(NOAIRAllDataF, .(Month), summarise, cost=abs(sum(Amount)))
ggplot(NOAIRSum, aes(Month, cost, group=1)) +
  geom_smooth(method="loess", se=F) + geom_point() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.position="none") +
  labs(x="", y="Monthly total ($)")

# Export to Excel
write.csv(AllDataF, file = "AllDataF.csv")
