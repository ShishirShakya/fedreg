Here are two codes: first is to generate the subwayplot based on the industry interactions second is the data management codes (written in R and partially in Python) to capture the networks of industry and federal regulations. I use Tableau to visualize the managed dataset. The Tableau visualization is given here: https://public.tableau.com/profile/shishir.shakya#!/vizhome/IndustrialNetworkOutputProductivityFederalRegulations/Dashboard1

# Subway Plot of industry interactions
## Install and load the packages
```
rm(list = ls())
dev.off(dev.list()['RStudioGD'])
ipack <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}

ipack(c('readxl', 'fpc', 'devtools', 'colorspace', 'circlize', 'reshape2', 'dplyr', 'tidyr', 'rstudioapi', 'readr'))
source('https://bioconductor.org/biocLite.R')
biocLite('BiocInstaller')
install_github('jokergoo/ComplexHeatmap')
library('ComplexHeatmap')
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
```

## Read the Excel Sheets
```
read_excel_allsheets <- function(fullpath) {
  sheets <- readxl::excel_sheets(fullpath)
  x <-    lapply(sheets, function(X) readxl::read_excel(fullpath, sheet = X))
  names(x) <- sheets
  x
}
```
## Especially Desigined to read 15 Sector Leontief Inverse Excel file of BEA
```
read.ImAI_sector <-function(mydata){
  colnames(mydata) <- mydata[5, ] # Use 4 for the shorcut name NAICS
  mydata <- mydata[7:21, -c(1,2)]
  mydata <- as.matrix(mydata)
  mydata <- apply(mydata, 2, function(x) as.numeric(x))
  colnames(mydata) <- c('Agriculture',	'Mining',	'Utilities',	'Construction',	'Manufacturing',
                        'Wholesale',	'Retail',	'Transport',	
                        'Information',	'Finance',	'Business',	'EHSA',
                        'Arts',	'Other services',	'Government')
  rownames(mydata) <- colnames(mydata)
  mydata
}

# BEA Data Downloads 
## Total Requirements
imai_sector <- 'https://www.bea.gov/industry/xls/io-annual/IxI_TR_1997-2015_Sector.xlsx'
if (file.exists('imai_sector.xlsx') == FALSE) { #  get the zip file
  download.file(imai_sector, destfile = 'imai_sector.xlsx', mode='wb')
}
imai_sector <- lapply(read_excel_allsheets('imai_sector.xlsx')[3:21], read.ImAI_sector)

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

# Quick Stats
outgen <- function(val){
  id <- c(1:nsec)
  t <- 1997:2015
  val_trend <- apply(val, 1, function(x) lm(x~t)$coefficients[2])
  val_avg <- apply(val, 1, mean)
  val_sd <- apply(val, 1, sd)
  rnam <- rownames(val)
  cnam <- c('id', 'Sectors', '1997',  '1998',  '1999', '2000',
            '2001',  '2002',  '2003',  '2004',  '2005',
            '2006',  '2007',  '2008',  '2009',  '2010',
            '2011',  '2012',  '2013',  '2014',  '2015', 'Average', 'StDev',
            'Trend')
  
  PGR_Val <- cbind(id, rnam, val, val_avg, val_sd, val_trend)
  colnames(PGR_Val) <- cnam
  rownames(PGR_Val) <- NULL
  return(PGR_Val)
}
```
## Subway Style Plot of Backward Linkages
```
bl_df <- bl(imai_sector)[, 1:21]
molten = reshape2::melt(bl_df, id = c('Sectors', 'id'))
tops <- nsec
dftags4 <- molten %>%
  group_by(variable) %>%
  mutate(rank = row_number(value)) %>%
  ungroup() %>%
  filter(rank <= tops) %>%
  mutate(rank = factor(rank, levels = seq(tops)),
         rval = as.numeric(value))

colors <- c('Manufacturing' = '#6a40fd', 'Agriculture' = '#198ce7', 'Transport' = '#563d7c', 'Construction' = '#f1e05a',
            'Information' = '#2f2b09', 'Arts' = '#b07219', 'Utilities' = '#e44b23', 'EHSA' = 'green',
            'Mining' = '#c128d5', 'Government' = 'grey', 'Business' = '#2dedd7', 'Finance' = '#14470d', 'Other services' = '#ed772d', 
            'Retail' = '#f7abb3', 'Wholesale' = '#2da3ed')

othertags <- dftags4 %>% distinct(Sectors) %>% filter(!Sectors %in% names(colors)) %>% .$Sectors

colors <- c(colors, setNames(rep('gray', length(othertags)), othertags))

library(ggplot2)
p <- ggplot(mapping = aes(variable, y = rank, group = Sectors, color = Sectors)) +
  geom_line(size = 1.7, alpha = 0.25, data = dftags4) +
  geom_line(size = 2.5, data = dftags4 %>% filter(Sectors %in% names(colors)[colors != 'gray'])) +
  geom_point(size = 4, alpha = 0.25, data = dftags4) +
  geom_point(size = 4, data = dftags4 %>% filter(Sectors %in% names(colors)[colors != 'gray'])) +
  geom_point(size = 1.75, color = 'white', data = dftags4) +
  #geom_text(data = dftags4, aes(label = Sectors), hjust = -0, size = 4.5) +
  #geom_text(data = dftags4, aes(label = Sectors), hjust = 1, size = 4.5) +
  scale_color_manual(values = colors) +
  ggtitle('The Evolution of Backward Linkages') +
  xlab('Years') 
p
```
![subwayplot](https://user-images.githubusercontent.com/23665666/39478815-762e0872-4d31-11e8-8722-2943a3112aa4.PNG)

## Data management for Tablaeu
```
ipak(c('dplyr', 'readxl', 'readr', 'reshape2', 'igraph', 'plm', 'rstudioapi'))
dir.create(file.path(path, "rawdata"))
dir.create(file.path(path, "rawdata/input-output"))
myurl <- 'https://www.bls.gov/emp/input-output/input-output.zip'
if (file.exists('rawdata/input-output/input-output.zip') == FALSE) { # get the zip file
  download.file(myurl, destfile = "rawdata/input-output/input-output.zip", mode="wb")
}
unzip('rawdata/input-output/input-output.zip', exdir=paste(path, '/rawdata/input-output', sep=""))
```
## Aggregation Function
```
aggregate_mat <- function(mat, row_name_mat, col_name_mat){
  M <- as.matrix(mat)
  row_name_mat <- as.matrix(row_name_mat)
  col_name_mat <- as.matrix(col_name_mat)
  r <- row_name_mat[ ,1]
  c <- t(col_name_mat[ ,1])
  
  rownames(M) <- r
  colnames(M) <- c
  
  M <- t(sapply(by(M,rownames(M),colSums),identity))
  M <- t(M)
  M <- sapply(by(M,rownames(M),colSums),identity)
  df <- as.data.frame(M)
  df <- df[order(rownames(df)), ]
  
  df <- df[ ,order(colnames(df))]
  return(df)
}
```

## Generate Inter-Industry Flow
```
## A. Load Use, Make, Valuadded, Commodity FinalDemand, and Aggregated Commodity Final Demand
## B. Aggregate above matrices based on the Unique aggregation scheme
## C. Perform required consistency Checks
## D. Solve for B, D and A matrix as per : https://www.bea.gov/papers/pdf/IOmanual_092906.pdf or https://www.bea.gov/industry/pdf/total-requirements-derivation.pdf (latest)
## E. Check consistency of B and D matrix if they can generate back q and x matrix or not.
## F. Generate A matrix based on Industry by Industry Technological Assumptions
## G. Generate Z matrix (inter-industry flow)
years <- as.character(c(1997:2014))
for (i in years){
  # Intermediate portion of the use matrix in which the column shows for a given industry the amount of each commodity it uses, including noncomparable imports and used and secondhand goods.  This is a commodity-by-industry matrix. 
  U <- as.matrix(readr::read_csv(paste0("rawdata/input-output/ioreal/REAL_USE/REAL_USE_",i,".csv"), 
                                 col_types = cols(Sector = col_skip()))[c(1:206), -207])
  
  # Make matrix in which the column shows for a given commodity the amount produced in each industry.  This is an industry-by-commodity matrix.  V has columns showing only zero entries for noncomparable imports and used and secondhand goods. 
  V <- as.matrix(readr::read_csv(paste0("rawdata/input-output/ioreal/REAL_MAKE/REAL_MAKE_",i,".csv"), 
                                 col_types = cols(Sector = col_skip())))
  
  
  FD <- as.matrix(readr::read_csv(paste0("rawdata/input-output/ioreal/REAL_FDAGG/REAL_FDAGG_",i,".csv"), 
                                  col_types = cols(Sector = col_skip())))
  
  # Total Value addition is a row vector in which each entry shows the total value addition by each industry.
  va <- as.matrix(readr::read_csv(paste0("rawdata/input-output/ioreal/REAL_USE/REAL_USE_",i,".csv"),  
                                  col_types = cols(Sector = col_skip()))[c(208, 209,210), c(1:206)])
  
  # A column vector in which each entry shows the total final demand purchases for each commodity from the use table. 
  e <- as.matrix(readr::read_csv(paste0("rawdata/input-output/ioreal/REAL_USE/REAL_USE_",i,".csv"),  
                                 col_types = cols(Sector = col_skip()))[c(1:206), 207]) #e is final demand of commoditites.
  e <- as.data.frame(e)
  
  # Aggregation Scheme
  uagg_r <- readxl::read_excel("rawdata/agg-scheme/agg_scheme.xlsx", sheet = "uagg")[ c(1:206),15]
  uagg_c <- readxl::read_excel("rawdata/agg-scheme/agg_scheme.xlsx", sheet = "uagg")[ c(1:206),15]
  vagg <- readxl::read_excel("rawdata/agg-scheme/agg_scheme.xlsx", sheet = "vagg")[,15]
  fdagg <- readxl::read_excel("rawdata/agg-scheme/agg_scheme.xlsx", sheet = "fdagg")[,6]
  va_agg_r <- readxl::read_excel("rawdata/agg-scheme/agg_scheme.xlsx", sheet = "uagg")[ c(208:210),15]
  
  U <- as.matrix(aggregate_mat(U, uagg_r, uagg_c))
  V = as.matrix(aggregate_mat(V, vagg, vagg))
  FD = as.matrix(aggregate_mat(FD, vagg, fdagg))
  
  #Some row and column name adjusments for va
  va <- as.matrix(aggregate_mat(va, va_agg_r, uagg_c))
  va <- colSums(va); va <- as.data.frame(va) 
  va <- t(as.matrix(va))
  
  #Some row and column name adjusments for va
  e <- cbind(uagg_r, e)
  e <- e %>% group_by(RegInd) %>% summarise_all(funs(sum(.))) %>% as.data.frame()
  e <- as.data.frame(e[ ,2])
  colnames(e) <- 'e'
  rownames(e) <- rownames(U)
  e <- as.matrix(e)
  
  #rm(uagg_r, uagg_c, vagg, fdagg, va_agg_r)
  
  # A column vector in which each entry shows the total amount of each industry's output.  
  g <- as.matrix(t(colSums(U)+va))
  g <- as.matrix(ifelse(g == 0, 10^(-10), g))
  
  # A column vector in which each entry shows the total amount of each commodity's output. 
  q <- as.matrix(rowSums(U)+e)
  q <- as.matrix(ifelse(q == 0, 10^(-10), q))
  # Check the consistency of final demand matrix and total final demand.
  chk1 <- function(FinalDemandMatrix, TotalFinalDemandVector){
    FinalDemandMatrix <- as.matrix(FinalDemandMatrix)
    TotalFinalDemandVector <- as.matrix(TotalFinalDemandVector)
    diff <- rowSums(FinalDemandMatrix) - TotalFinalDemandVector
    if(all(abs(diff) < 0.01) == TRUE){
      print(paste0("chk1 confirmed for year ", i)) #Consistency of row sums of disaggregated final demand matrix is same as total final demand from Use matrix for year"
    }
    else{
      print(paste0("chk1 is not confirmed for year", i))
    }
  }
  chk1(FD, e)
  
  # Check the consistency of final demand matrix and total final demand.
  chk2 <- function(Ua, Va, E, VA){
    g_u <- as.matrix(t(colSums(Ua)+VA))
    q_u <- as.matrix(rowSums(Ua)+E)
    g_v <- as.matrix(rowSums(Va))
    q_v <- as.matrix(colSums(Va))
    
    z1 <- round(q_u-q_v)
    z2 <- round(g_u-g_v)
    
    if (all(abs(z1) < .Machine$double.eps) == TRUE){
      
    }
    
    if(all(abs(z2) < .Machine$double.eps) == TRUE){
      
      print(paste0("chk2 confirmed for year ", i)) #Consistency of IO is confirmed for year 
    }
    else{
      rint(paste0("chk2 is not confirmed for year ", i))
    }
    return(list(g=g_u, q = q_u))
    
  }
  consistency <- chk2(U, V, e, va)
  
  # Function to change the vector to a diagonal matrix where offdiagonals are zero and vector exits in diagonals.
  vec2diag = function (x){
    x <- as.matrix(x)
    if (nrow(x) != 1 && ncol(x) != 1) {
      stop("argument must be a row or column vector")
    }
    if (nrow(x) * ncol(x) == 1) {
      return(x)
    }
    else {
      return(as.matrix(diag(as.numeric(x))))
    }
  }
  
  # Direct input coefficients matrix in which entries in each column show the amount of a commodity used by an industry per dollar of output of that industry.  This is a commodityby-industry matrix.
  B <- U %*% vec2diag(1/g) # or We can use as #B <- as.matrix(Use) %*% solve(vec2diag(g)) # However this feedbacked the errors.
  
  # Check that both formula gives same results
  # U1 <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3); U1
  # g1 <- as.matrix(c(1,2,3), nrow=3); g1
  # g1/g1
  # B1 <- as.matrix(U1) %*% solve(vec2diag(g1)); B1
  # B2 <- as.matrix(U1) %*% vec2diag(1/g1); B2
  # rm(U1, g1, B1, B2)
  
  # A matrix in which entries in each column show, for a given commodity, the proportion of the total output of that commodity produced in each industry.  In the model, it is assumed that each commodity is produced by the various industries in fixed proportions.  This is an industry-by-commodity matrix.  D is also referred to as the market share matrix or transformation matrix. 
  D <- V %*% vec2diag(1/q) # D <- V %*% solve(vec2diag(q))
  
  # Check Consistency of g and q
  # Check Consistency of g and q
  chk3 <- function(Ua, Va, E, VA, fd, q, g, Bmat, Dmat){
    #Bmat <- as.matrix(ifelse(Bmat == 0, 10^(-10), Bmat))
    #Dmat <- as.matrix(ifelse(Dmat == 0, 10^(-10), Dmat))
    
    q1 <- rowSums(Ua) + E
    q2 <- (Bmat %*% g) + E
    
    Imat <- diag(1, nrow(V))
    CbyC <- solve(Imat - Bmat %*% Dmat)
    q3 <- CbyC %*% E
    
    g1 <- rowSums(Va)
    g2 <- Dmat %*% q
    
    IbyI <- solve(Imat-Dmat%*%Bmat)
    g3 <- IbyI %*% Dmat %*% E
    
    IbyC <- Dmat %*% CbyC
    g4 <- IbyC %*% E
    
    df <- as.data.frame(cbind(q1-q2,
                              q1-q3,
                              g1-g2,
                              g1-g3,
                              g1-g4))
    colnames(df) <- c('q1-q2', 'q1-q3', 'g1-g2', 'g1-g3', 'g1-g4')
    
    Amat <- Dmat%*%Bmat
    f <- Dmat %*% E
    
    if(all(abs(df) < 0.1) == TRUE){
      print(paste0("Consistency is confirmed for year", i))
    }
    else{
      print(paste0("Consistency is not confirmed for year", i))
    }
    
    
    
    results <- list(q1=q1, q2=q2, CbyC=CbyC, q3=q3, g1=g1, g2=g2, IbyI=IbyI, g3=g3, 
                    IbyC=IbyC, g4=g4, df=df, Amat = Amat, f = f, fd=fd)
    return(results)
    
  }
  
  Results <- chk3(U, V, e, va, FD, q, g, B, D)
  
  tech_coef <- Results$Amat
  colnames(tech_coef) <- rownames(tech_coef)
  write.csv(tech_coef, paste0('technical_coefficient_',i,'.csv'))
  
  Z <- tech_coef %*% vec2diag(g)
  colnames(Z) <- rownames(Z)
  write.csv(Z, paste0('Inter-IndusryFlow_',i,'.csv'))
  
  Ind_finalDemand <- Results$f
  colnames(Ind_finalDemand) <- 'Ind_finalDemand'
  write.csv(Ind_finalDemand, paste0('Ind_finalDemand_',i,'.csv'))
  
  Ind_output <- Results$g3
  colnames(Ind_output) <- 'Ind_output'
  rownames(Ind_output) <- rownames(Z)
  write.csv(Ind_output, paste0('Ind_output_',i,'.csv'))
  
  FinalDemand <- Results$fd
  #colnames(Ind_output) <- 'Ind_output'
  rownames(FinalDemand) <- rownames(Z)
  write.csv(FinalDemand, paste0('FinalDemand_',i,'.csv'))
  
}

rm(B, D, e, fdagg, FinalDemand, g, Ind_finalDemand, Ind_output,q,V,FD,
   tech_coef, U, uagg_c, uagg_r, v, va, va_agg_r, vagg, Z, consistency, i, years,
   aggregate_mat, chk1, chk2, chk3, vec2diag)
```

## Get the network properties of inter-sectorial trade
```
fileList <- list.files(path=path, pattern="Inter-IndusryFlow")
my_csv_read <-function(mat){
  mat <- mat[ ,-1]
  mat <- mat[ c(1:22),c(1:22)]
  rownames(mat) <- colnames(mat)
  return(mat)
}
df <- lapply(fileList, read.csv)
#names(df) <- fileList
df <- lapply(df, my_csv_read)
Ind_names <- rownames(as.data.frame(df[1]))
year <- as.character(c(1997:2014))

NetworkMetrics <- list()
for (i in 1:length(df)){
  A <- as.matrix(as.data.frame(df[i]))
  g <- igraph::graph.adjacency(A, mode="directed", weighted=TRUE)
  metrics <- list(closeness_In <- closeness(g, mode='in',  normalized = TRUE),
                  closeness_Out  <- closeness(g, mode='out',  normalized = TRUE),
                  closeness_Total  <- closeness(g, mode='total',  normalized = TRUE),
                  Transitivity <- transitivity(g, type="weighted"),
                  Betweenness <- betweenness(g, normalized = TRUE),
                  Eigenvector=eigen_centrality(g, scale=FALSE)$vector,
                  PageRank=page_rank(g)$vector,
                  AS <- authority_score(g, scale = FALSE)$vector,
                  HS <- hub_score(g,  scale = FALSE)$vector)
  
  metrics_df <- do.call(cbind.data.frame, metrics)
  colnames(metrics_df) <- c('closeness_In', 'closeness_Out', 'closeness_Total',
                            'Transitivity', 'Betweenness', 'Eigenvector', 'PageRank', 'Authority', 'Hub')
  metrics_df$year <- year[i]
  metrics_df$Ind <- Ind_names
  NetworkMetrics[[i]] <- metrics_df
}

NetworkMetrics_df <- do.call(rbind.data.frame, NetworkMetrics)
NetworkMetrics_df <- NetworkMetrics_df[order(NetworkMetrics_df$year, NetworkMetrics_df$Ind),]
rm(metrics_df, A, AS, Betweenness, closeness_In, closeness_Out, closeness_Total, 
   fileList, g, HS, i, metrics, Transitivity, year, NetworkMetrics, my_csv_read)
```

## Industry outputs, Imports, Domestic demands
```
fileList <- list.files(path=path, pattern="Ind_output_")
df <- lapply(fileList, read.csv)
year <- as.character(c(1997:2014))
Ind_output <- list()
for(i in 1:length(df)){
  A <- as.data.frame(as.matrix(as.data.frame(df[i])))
  A <- A[c(1:22), ]
  A$year <- as.character(year[i])
  colnames(A)[1] <- 'Ind'
  Ind_output[[i]] <- A
}
Ind_output <- do.call(rbind.data.frame, Ind_output)
Ind_output <- Ind_output[order(Ind_output$year, Ind_output$Ind),]

rm(fileList, df, year, A)

fileList <- list.files(path=path, pattern="FinalDemand_")
df <- lapply(fileList, read.csv)
my_csv_read <-function(mat){
  mat <- as.matrix(as.data.frame(mat))
  mat <- mat[c(1:22) ,c(1,6) ]
  colnames(mat) <- c('Ind', 'Imports')
  return(mat)
}

df <- lapply(df, my_csv_read)
year <- as.character(c(1997:2014))

Imp <- list()
for(i in 1:length(df)){
  A <- as.data.frame(as.matrix(as.data.frame(df[i])))
  A$year <- as.character(year[i])
  Imp[[i]] <- A
}
Imp <- do.call(rbind.data.frame, Imp)
Imp <- Imp[order(Imp$year, Imp$Ind),]

rm(fileList, df, year, A, my_csv_read)

fileList <- list.files(path=path, pattern="FinalDemand_")
df <- lapply(fileList, read.csv)
my_csv_read <-function(mat){
  mat <- as.matrix(as.data.frame(mat))[c(1:22), ]
  mat_cname <- mat[,1]
  mat <- mat[,-c(1,6)]
  mat <- apply(mat, 2, as.numeric)
  mat <- rowSums(mat)
  mat <- cbind(mat_cname, mat)
  colnames(mat) <- c('Ind', 'DomFD')
  return(mat)
}

df <- lapply(df, my_csv_read)
year <- as.character(c(1997:2014))

DomFD <- list()
for(i in 1:length(df)){
  A <- as.data.frame(as.matrix(as.data.frame(df[i])))
  A$year <- as.character(year[i])
  DomFD[[i]] <- A
}
DomFD <- do.call(rbind.data.frame, DomFD)
DomFD <- DomFD[order(DomFD$year, DomFD$Ind),]
rm(fileList, df, year, A, my_csv_read)

panel_data <- cbind(NetworkMetrics_df, Ind_output, DomFD, Imp)
panel_data <- panel_data[ , -c(12,14,15,17,18,20)]
rownames(panel_data) <- NULL

rm(NetworkMetrics_df, Ind_output, DomFD, Imp)
````

## Scraping regdata
```
## Two Level NAICS
regdata_II <- "https://s3.amazonaws.com/regdata-gold/regdata/2.2/20-08-2015/extracts/regdata_by_2-digit_industry.csv"
## Three Lelve NAICS
regdata_III <- "https://s3.amazonaws.com/regdata-gold/regdata/2.2/20-08-2015/extracts/regdata_by_3-digit_industry.csv"
## FOUR Level NAICS
regdata_IV <- "https://s3.amazonaws.com/regdata-gold/regdata/2.2/20-08-2015/extracts/regdata_by_4-digit_industry.csv"
## BEA level NAICS
regdata_BEA <- "https://s3.amazonaws.com/regdata-gold/regdata/2.2/20-08-2015/extracts/regdata_by_bea_industry.csv"

myurls <- c(regdata_II, regdata_III, regdata_IV, regdata_BEA)
reg_data_name <-  c('regdata_II', 'regdata_III', 'regdata_IV',  'regdata_BEA')
##########

for(i in 1:length(myurls)){
  regdata <- read.csv(url(as.character(myurls[i])), header =T)
  regdata$industry <- sub("^", "Ind", regdata$industry )
  regdata_index <- regdata[ ,-4]
  regdata_word <- regdata[ ,-3]
  head(regdata_index)
  head(regdata_word)
  regdata_index <- dcast(data = regdata_index,formula = year~industry)
  regdata_word <- dcast(data = regdata_word,formula = year~industry)
  write.csv(regdata_index, paste0('index_', as.character(reg_data_name[i]),'.csv'))
  write.csv(regdata_word, paste0('word_', as.character( reg_data_name[i]), '.csv'))
  rm(regdata, regdata_index, regdata_word)
}

fileList <- list.files(path=path, pattern="word_")
regdf <- lapply(fileList, read.csv)
names(regdf) <- fileList

regdata <- cbind(regdf$word_regdata_II.csv$year, regdf$word_regdata_II.csv$Ind11,
                 regdf$word_regdata_II.csv$Ind21, regdf$word_regdata_II.csv$Ind22,
                 regdf$word_regdata_II.csv$Ind23)

Ind31 <- as.data.frame(cbind(regdf$word_regdata_III.csv$Ind311, regdf$word_regdata_III.csv$Ind313, regdf$word_regdata_III.csv$Ind314))
Ind31 <- apply(Ind31, 1, sum)

Ind32 <- as.data.frame(cbind(regdf$word_regdata_III.csv$Ind322, regdf$word_regdata_III.csv$Ind324, 
                             regdf$word_regdata_III.csv$Ind325, regdf$word_regdata_III.csv$Ind327))
Ind32 <- apply(Ind32, 1, sum)

Ind33 <- as.data.frame(cbind(regdf$word_regdata_III.csv$Ind331, regdf$word_regdata_III.csv$Ind333, 
                             regdf$word_regdata_III.csv$Ind334, regdf$word_regdata_III.csv$Ind335, 
                             regdf$word_regdata_III.csv$Ind336, regdf$word_regdata_III.csv$Ind337,
                             regdf$word_regdata_III.csv$Ind339))
Ind33 <- apply(Ind33, 1, sum)

regdata <-cbind(regdata, Ind31, Ind32, Ind33, regdf$word_regdata_II.csv$Ind42) 

Ind44_45 <- as.data.frame(cbind(regdf$word_regdata_III.csv$Ind444, regdf$word_regdata_III.csv$Ind446, 
                                regdf$word_regdata_III.csv$Ind447, regdf$word_regdata_III.csv$Ind452, 
                                regdf$word_regdata_III.csv$Ind454))
Ind44_45 <- apply(Ind44_45, 1, sum)

Ind48 <- as.data.frame(cbind(regdf$word_regdata_III.csv$Ind481, regdf$word_regdata_III.csv$Ind482, 
                             regdf$word_regdata_III.csv$Ind483, regdf$word_regdata_III.csv$Ind484, 
                             regdf$word_regdata_III.csv$Ind486, regdf$word_regdata_III.csv$Ind487,
                             regdf$word_regdata_III.csv$Ind488))
Ind48 <- apply(Ind48, 1, sum)

Ind49 <- as.data.frame(cbind(regdf$word_regdata_III.csv$Ind492, regdf$word_regdata_III.csv$Ind493))
Ind49 <- apply(Ind49, 1, sum)

regdata <-cbind(regdata, Ind44_45, Ind48, Ind49)

Ind_B <- cbind(regdf$word_regdata_II.csv$Ind51,
               regdf$word_regdata_II.csv$Ind52, regdf$word_regdata_II.csv$Ind53,
               regdf$word_regdata_II.csv$Ind54, regdf$word_regdata_II.csv$Ind55,
               regdf$word_regdata_II.csv$Ind56, regdf$word_regdata_II.csv$Ind61,
               regdf$word_regdata_II.csv$Ind62, regdf$word_regdata_II.csv$Ind71)

regdata <-cbind(regdata, Ind_B)

Ind81 <- as.data.frame(cbind(regdf$word_regdata_IV.csv$Ind8114, regdf$word_regdata_IV.csv$Ind8131, 
                             regdf$word_regdata_IV.csv$Ind8141))
Ind81 <- apply(Ind81, 1, sum)

regdata <- cbind(regdata, Ind81, regdf$word_regdata_II.csv$Ind92)
rm(Ind31, Ind32, Ind33, Ind44_45, Ind48, Ind49, Ind_B, Ind81)

colnames(regdata) <- c('year', Ind_names)

regdata <- as.data.frame(regdata[-c(1:27) ,])
regdata$id <- 'id'

regdata <- melt(data = regdata, id=c("year"))[c(1:396), ]
colnames(regdata ) <- c('year', 'Ind', 'Restrictions')
regdata <- regdata[order(regdata$year, regdata$Ind),]


panel_data <- cbind(panel_data, regdata)
panel_data <- panel_data[,-c(15,16)]

rm(regdata, fileList, i, myurl, myurls, regdata_BEA, 
   regdata_II, regdata_III, regdata_IV, regdf, reg_data_name)
```

## Scraping Labor Employment and Compensation in python (To be run with Python only)
```
# -*- coding: utf-8 -*-
"""
Created on Sat Sep 16 08:34:11 2017

@author: Shishir Shakya
"""
import pandas as pd
import numpy as np
import os, zipfile

## Data management of Quarterly Census of Employment and Wages
if not os.path.exists('rawdata/qcew'):
  os.makedirs('rawdata/qcew')

if not os.path.exists('rawdata/qcew/legacy_flat_files'):
  os.makedirs('rawdata/qcew/legacy_flat_files')

#This code will download almost 4GB of legacy flat file data on employment.
folder = 'rawdata/qcew/legacy_flat_files/'
years = range(1997, 2017)
names = ['%02d_all_enb.zip'%i for i in years]
#for i in range(0,len(names)):
#    path = folder + names[i]
#    if not os.path.exists(path):
#        urllib.urlretrieve('https://www.bls.gov/cew/data/files/'+str(years[i])+'/enb/'+names[i], path)


#From each of the downloaded zip file we only need the file from the folder called national of the zip file
for i in range(0,len(names)):
  path = folder + names[i]
archive = zipfile.ZipFile(path)
for file in archive.namelist():
  if file.startswith('national/'):
  archive.extract(file, 'rawdata/qcew')



folder = 'rawdata/qcew/national/'
years = range(97,100)+range(0,15)
names = ['NT00US%02d.ENB'%i for i in years]
# define the column names
col = ['Industry code', 'Year','Annual Average Employment','Total Annual Wages']
# read data
rawdata = []
for i in range(0,len(names)):
  path = folder + names[i]
rawdata.append(pd.read_fwf(path, colspecs = [(11,17),(17,21),(380,389),(389,404)], header = None, index_col = 1, names = col, skiprows = 4))
## The first 4 rows are deleted from the dataset because they are not included in the Area code ('US000'), or the Ownership ([1,2,3,5]), or the Aggregation Level ('1.') 
###############################################################################
# Create the industry sector list from the industry code
ind_plan = {1: ['11'], # 1.Agriculture, Forestry, Fishing and Hunting sector 
  2: ['211', '2121', '2122', '2123', '213'], # 2.Mining, Quarrying, and Oil and Gas Extraction
  3: ['2211', '2212', '2213'], # 3. Utilities Sectors
  4: ['23'], # 4. Construction sector 
  5: ['311','312','313','314','315','316','322','323'], # 14. Other non-durable manufacturing, # 5. Support activities for mining
  6: ['321','327','324', '325','326' ], # 6. Electric power generation, transmission, and distribution
  7: ['331', '332', '333', '336', '334','335','337','339'], # 7. Natural gas distribution
  8: ['42'], # 8. Water, sewage, and other system
  9: ['44-45'], # 9. Construction
  10: ['481','482','483', '484', '486','485','487','488'], # 10. Primary and fabricated metal products
  11: ['491','492', '493'], # 11. Machinery
  12: ['51'], # 12. Motor vehicles and other transportation equipment
  13: ['52'], # 13. Other durable manufacturing
  14: ['53'],
  15: ['54'], # 15. Petroleum and coal products  
  16: ['55'], # 16. Chemical, plastics, and rubber products
  17: ['56'], # 17. Wholesale trade
  18: ['61'], # 18. Retail trade
  19: ['62'], # 19. Air, rail, and water transportation
  20: ['71','72'], # 20. Truck transportation
  21: ['81'], # 21. Pipeline transportation   
  22: ['92'], # 22. Transit and sightseeing transportation and transportation support services
} 
###############################################################################
# Group the rawdata of different ownerships by the industry code 
# ENB files of old ECIO version and Newset both were checked with the WinMerge software. Their values are same.
# del folder, years, names, col, i, path, file

EPIO = []
CPIO = []
for i in range(len(rawdata)):
  rd = rawdata[i]
for j in range(len(rd['Industry code'])):
  for keys, values in ind_plan.iteritems():
  if rd['Industry code'].iloc[j] in values:
  rd['Industry code'].iloc[j] = keys
newrd = rd.loc[rd['Industry code'].isin(list(range(1,23)))]    
frd = newrd.groupby('Industry code').sum()
# Read the EPIO and CPIO from the dataset
EPIO_raw = frd.iloc[:,0]
CPIO_raw = frd.iloc[:,1]                 
EPIO.append(EPIO_raw)
CPIO.append(CPIO_raw)
# Group the EPIO and CPIO dataset    
EMPIO = pd.DataFrame(pd.concat(EPIO[:]).reshape(len(rawdata),22)).T
CMPIO = pd.DataFrame(pd.concat(CPIO[:]).reshape(len(rawdata),22)).T
# Calculate CMPEMPIO                    
EMPIO = EMPIO/1000
CMPIO = CMPIO/1000000
CMPEMPIO = np.divide(CMPIO,EMPIO)

pd.DataFrame(EMPIO).to_csv('EMPIO.csv',header = False, index = False)
pd.DataFrame(CMPIO).to_csv('CMPIO.csv',header = False, index = False)
pd.DataFrame(CMPEMPIO).to_csv('CMPEMPIO.csv',header = False, index = False)
```


##  EMPIO, CMPIO, CMPEMPIO in Panel_data
```
bls <- function(path, Ind_name, name, def){
  df <- t(read.csv(path, header=F))
  colnames(df) <- Ind_names
  year <- as.matrix(c(1997:2014))
  df <- cbind(df, year)
  colnames(df)[23] <- 'year'
  df <- as.data.frame(df)
  df$id <- 'id'
  df <- melt(data = df, id=c("year"))[c(1:396), ]
  colnames(df)[3] <- name
  colnames(df)[2] <- 'Ind'
  df <- df[order(df$Ind),]
  
  return(df)
}

EMPIO <- bls('rawdata/BLS/EMPIO.csv',Ind_name, 'empio' )
CMPIO <- bls('rawdata/BLS/CMPIO.csv',Ind_name, 'cmpio' )

def <- as.numeric(as.matrix(read_excel('rawdata/deflator/PCEPI.xls', skip=10)[ ,2])) #Load the deflator
def <- rep_len(def, length.out = 396)

CMPIO$cmpio <- as.numeric(CMPIO$cmpio)
CMPIO$cmpio <- CMPIO$cmpio/def*100


CMPEMPIO <- EMPIO
CMPEMPIO$cmpempio <- CMPIO$cmpio / as.numeric(EMPIO$empio)
CMPEMPIO <- CMPEMPIO[ ,-3]

EMPIO <- EMPIO[order(EMPIO$year),]
CMPIO <- CMPIO[order(CMPIO$year),]
CMPEMPIO <- CMPEMPIO[order(CMPEMPIO$year),]


blsdf <- cbind(EMPIO, CMPIO, CMPEMPIO)
blsdf <- blsdf[,c(3,6,9)]

panel_data <- cbind(panel_data, blsdf)

write.csv(panel_data, 'panel_data.csv')

rm(blsdf, EMPIO, CMPIO, CMPEMPIO, bls, panel_data)
```

## Data Management for Modeling
```
Panel <- read_csv('panel_data.csv')[,-1]
Panel$ImpA <- Panel$Imports + abs(min(Panel$Imports)+1)

as.matrix(names(Panel))
id_df <- Panel[,c(10,11)]
network_metrics <- Panel[,c(1:9)]
economy <- Panel[,c(13,19,16,17,18)]; economy <- as.data.frame(apply(economy,2, log))
regulation <- Panel[ ,c(15)]; regulation <- as.data.frame(apply(regulation,2, log))
IndustryOutput <- log(Panel[,c(12)])

pdf <- cbind(id_df, IndustryOutput, economy, network_metrics, regulation)
write.csv(pdf, 'Panel_data_transformation.csv')

rm(Panel,id_df, IndustryOutput, economy, network_metrics, regulation)
```

## Tableau Visualization
The Tableau visualization is given here: https://public.tableau.com/profile/shishir.shakya#!/vizhome/IndustrialNetworkOutputProductivityFederalRegulations/Dashboard1
