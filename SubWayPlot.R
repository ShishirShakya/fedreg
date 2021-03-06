# Clean the global enviroment
rm(list = ls())
dev.off(dev.list()['RStudioGD'])
# Install and load the packages
ipack <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
ipack(c('rstudioapi', 'readr'))
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

ipack(c("readxl", "fpc", "devtools", "colorspace", "circlize", "reshape2", "dplyr", "tidyr"))
source("https://bioconductor.org/biocLite.R")
#biocLite("BiocInstaller")
#install_github("jokergoo/ComplexHeatmap")
library("ComplexHeatmap")

# Read the Excel Sheets
read_excel_allsheets <- function(fullpath) {
  sheets <- readxl::excel_sheets(fullpath)
  x <-    lapply(sheets, function(X) readxl::read_excel(fullpath, sheet = X))
  names(x) <- sheets
  x
}
# Especially Desigined to read 15 Sector Leontief Inverse Excel file of BEA
read.ImAI_sector <-function(mydata){
  colnames(mydata) <- mydata[5, ] # Use 4 for the shorcut name NAICS
  mydata <- mydata[7:21, -c(1,2)]
  mydata <- as.matrix(mydata)
  mydata <- apply(mydata, 2, function(x) as.numeric(x))
  colnames(mydata) <- c("Agriculture",	"Mining",	"Utilities",	"Construction",	"Manufacturing",
                        "Wholesale",	"Retail",	"Transport",	
                        "Information",	"Finance",	"Business",	"EHSA",
                        "Arts",	"Other services",	"Government")
  rownames(mydata) <- colnames(mydata)
  mydata
}

# BEA Data Downloads 
# Total Requirements
imai_sector <- "https://www.bea.gov/industry/xls/io-annual/IxI_TR_1997-2015_Sector.xlsx"
if (file.exists('imai_sector.xlsx') == FALSE) { #  get the zip file
  download.file(imai_sector, destfile = "imai_sector.xlsx", mode="wb")
}
imai_sector <- lapply(read_excel_allsheets("imai_sector.xlsx")[3:21], read.ImAI_sector)

nsec <- 15
## Backward Linkages 
bl <- function(listy_mat){
  z <- function(x){
    z <- colSums(x)
  }
  listy <- as.list(lapply(listy_mat, z))
  df <- as.data.frame(cbind(listy$`1997`, listy$`1998`, listy$`1999`,listy$`2000`,
                            listy$`2001`, listy$`2002`, listy$`2003`, listy$`2004`, listy$`2005`,
                            listy$`2006`, listy$`2007`, listy$`2008`, listy$`2009`, listy$`2010`,
                            listy$`2011`, listy$`2012`, listy$`2013`, listy$`2014`, listy$`2015`))
  result <- outgen(df)
  return(result)
}

#Generate output with Average, SD and Trend coefficient.
#######################################
outgen <- function(val){
  id <- c(1:nsec)
  t <- 1997:2015
  val_trend <- apply(val, 1, function(x) lm(x~t)$coefficients[2])
  val_avg <- apply(val, 1, mean)
  val_sd <- apply(val, 1, sd)
  rnam <- rownames(val)
  cnam <- c("id", "Sectors", "1997",  "1998",  "1999", "2000",
            "2001",  "2002",  "2003",  "2004",  "2005",
            "2006",  "2007",  "2008",  "2009",  "2010",
            "2011",  "2012",  "2013",  "2014",  "2015", "Average", "StDev",
            "Trend")
  
  PGR_Val <- cbind(id, rnam, val, val_avg, val_sd, val_trend)
  colnames(PGR_Val) <- cnam
  rownames(PGR_Val) <- NULL
  return(PGR_Val)
}

#######################################
# SUbway Style Plot of Backward Linkages
########################################
bl_df <- bl(imai_sector)[, 1:21]
molten = reshape2::melt(bl_df, id = c("Sectors", "id"))
tops <- nsec
dftags4 <- molten %>%
  group_by(variable) %>%
  mutate(rank = row_number(value)) %>%
  ungroup() %>%
  filter(rank <= tops) %>%
  mutate(rank = factor(rank, levels = seq(tops)),
         rval = as.numeric(value))

colors <- c("Manufacturing" = "#6a40fd", "Agriculture" = "#198ce7", "Transport" = "#563d7c", "Construction" = "#f1e05a",
            "Information" = "#2f2b09", "Arts" = "#b07219", "Utilities" = "#e44b23", "EHSA" = "green",
            "Mining" = "#c128d5", "Government" = "grey", "Business" = "#2dedd7", "Finance" = "#14470d", "Other services" = '#ed772d', 
            "Retail" = "#f7abb3", "Wholesale" = "#2da3ed")

othertags <- dftags4 %>% distinct(Sectors) %>% filter(!Sectors %in% names(colors)) %>% .$Sectors

colors <- c(colors, setNames(rep("gray", length(othertags)), othertags))

library(ggplot2)
p <- ggplot(mapping = aes(variable, y = rank, group = Sectors, color = Sectors)) +
  geom_line(size = 1.7, alpha = 0.25, data = dftags4) +
  geom_line(size = 2.5, data = dftags4 %>% filter(Sectors %in% names(colors)[colors != "gray"])) +
  geom_point(size = 4, alpha = 0.25, data = dftags4) +
  geom_point(size = 4, data = dftags4 %>% filter(Sectors %in% names(colors)[colors != "gray"])) +
  geom_point(size = 1.75, color = "white", data = dftags4) +
  #geom_text(data = dftags4, aes(label = Sectors), hjust = -0, size = 4.5) +
  #geom_text(data = dftags4, aes(label = Sectors), hjust = 1, size = 4.5) +
  scale_color_manual(values = colors) +
  ggtitle("The Evolution of Backward Linkages") +
  xlab("Years") 
p