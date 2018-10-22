# gather data together in a text file that will be read into Matlab for calculating center of biomass metrics 
# used in Henderson et al. 2017 and Nye et al. 2009


setwd('C:/Users/mehen/Dropbox (Nye lab)/NMFS data/')
# set your working directory.
setwd('')
load('Survdat_Nye.Rdata')

library(data.table)
options(scipen=100)  # to view all of ID variable

nye_fish = c(15, 23, 25, 26, 28, 32, 33, 72, 73, 74, 75, 76, 77, 78, 84, 101, 102, 103, 104, 105, 106, 108, 121, 139, 141, 155, 156, 163, 164, 192, 193, 197, 301, 311, 312, 313, 502, 503)
multi_stock_fish = c(72, 73, 74, 77, 105, 106)

survdat = data.frame(survdat) # convert to data.frame format.
bn_setup<-survdat[,c('ID', 'CRUISE6', 'STATION', 'STRATUM', 'EST_YEAR', 'EST_MONTH', 'EST_DAY', 'SEASON', 'BOTTEMP', 'SURFTEMP',
                     'AVGDEPTH', 'DECDEG_BEGLAT', 'DECDEG_BEGLON', 'SVSPP', 'CATCHSEX', 'BIOMASS', 'ABUNDANCE')]
bn_setup<-bn_setup[order(bn_setup$ID),]

# Select season and years
bn_setup<-bn_setup[bn_setup$SEASON=='SPRING',]
bn_setup<-bn_setup[bn_setup$EST_YEAR>=1968,]
bn_setup<-bn_setup[bn_setup$EST_YEAR<=2014,]

# Select strata
s1 = bn_setup[bn_setup$STRATUM>=01010 & bn_setup$STRATUM<=01300,]
s2 = bn_setup[bn_setup$STRATUM>=01360 & bn_setup$STRATUM<=01400,]
s3 = bn_setup[bn_setup$STRATUM>=01610 & bn_setup$STRATUM<=01760,]
bn_setup<-rbind(s1,s2,s3)

# Select species
bn_setup<-bn_setup[bn_setup$SVSPP %in% nye_fish,] 

# Select unique rows
bn_unique<-bn_setup[!duplicated(bn_setup[c("ID", "SVSPP", "CATCHSEX")]),]

# Add ecoregion column: N=northern, S=southern, NS=single-stock
bn_unique$ECOREGION <- 'NS'
multi_stock <- bn_unique[bn_unique$SVSPP==multi_stock_fish,]
head(multi_stock)
# build north, south, and single stock data.frames. then combine.
nstrat1 <- multi_stock[multi_stock$STRATUM==01240,]
nstrat2 <- multi_stock[multi_stock$STRATUM>=01260 & multi_stock$STRATUM<=01300,]
nstrat3 <- multi_stock[multi_stock$STRATUM>=01360 & multi_stock$STRATUM<=01400,] 
north <- rbind(nstrat1, nstrat2, nstrat3)
north$ECOREGION <- 'N'
ind_south <- which(is.na(match(multi_stock$ID, north$ID)))
south <- multi_stock[ind_south,]
south$ECOREGION <- 'S'
single_stock = bn_unique[!bn_unique$SVSPP==multi_stock_fish,]
bn_unique1 <- rbind(north, south, single_stock)
bn_unique1<-bn_unique1[order(bn_unique1$ID),]

# Sum abundances and biomasses over catchsex 
bn_unique2<-aggregate(cbind(BIOMASS, ABUNDANCE) ~ 
                        ID + SVSPP + ECOREGION, sum, data=bn_unique1)
bn_unique2<-bn_unique2[order(bn_unique2$ID),]
bn_unique3<-unique(bn_unique1[,c(1:13)])
bn_unique4<-merge(bn_unique2, bn_unique3, by="ID")
bn_unique4$SVSPP_ECOREGION<-paste0(bn_unique4$SVSPP,'_',bn_unique4$ECOREGION)
bn_unique5<-bn_unique4[,c('ID', 'CRUISE6', 'STATION', 'STRATUM', 'EST_YEAR', 'EST_MONTH', 'EST_DAY', 'SEASON', 'BOTTEMP', 'SURFTEMP', 'AVGDEPTH', 
                          'DECDEG_BEGLAT', 'DECDEG_BEGLON', 'SVSPP_ECOREGION', 'BIOMASS')]

# reformat the data
svspp<- c('15_NS','23_NS','25_NS','26_NS','28_NS','32_NS','33_NS','72_N','72_S','73_N','73_S','74_N','74_S','75_NS','76_NS','77_N','77_S','78_NS','84_NS','101_NS','102_NS','103_NS','104_NS','105_N','105_S','106_N', '106_S','108_NS','121_NS','139_NS','141_NS','155_NS','156_NS','163_NS','164_NS','192_NS','193_NS','197_NS','301_NS', '311_NS', '312_NS', '313_NS', '502_NS', '503_NS')
station_dat<-unique(bn_unique5[,c(1:13)])
merge_dat<-station_dat  # initialize the loop
for (s in svspp){
  fish_dat<-bn_unique5[bn_unique5$SVSPP_ECOREGION==s,c(1:13,15)]
  colnames(fish_dat)[14]<-s
  merge_dat = merge(merge_dat, fish_dat, by=names(station_dat), all=T) 
  na_index<-which(is.na(merge_dat[,dim(merge_dat)[2]]))
  merge_dat[,dim(merge_dat)[2]][na_index]<-0
  # log (biomass + 1)
  merge_dat[,dim(merge_dat)[2]]<-log10(merge_dat[,dim(merge_dat)[2]]+1)
}

# Log-transform biomass
merge_dat[,14:57]<-log10(merge_dat[,14:57]+1)

# save to csv for later
write.csv(merge_dat, 'COB prep for MATLAB.csv')

# SAVE TO TXT -- for MATLAB
# change na values to NaN for MATLAB
na_surf<-which(is.na(merge_dat$SURFTEMP))
merge_dat$SURFTEMP[na_surf]<-'NaN'
na_bot<-which(is.na(merge_dat$BOTTEMP))
merge_dat$BOTTEMP[na_bot]<-'NaN'
na_depth<-which(is.na(merge_dat$AVGDEPTH))
merge_dat$AVGDEPTH[na_depth]<-'NaN'
write.table(merge_dat, 'COB prep for MATLAB.txt', sep='\t', quote=F, row.names=F)
