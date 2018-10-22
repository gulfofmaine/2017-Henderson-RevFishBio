# code for stratified mean biomass used in Henderson et al. 2017 

setwd('') # set working directory path
load('data.Rdata') # bottom trawl data
load('stratum.area.Rdata') # list of areas for each stratum in the survey

library(data.table) # needed to manipulate data.table() 

# set list of stock codes from the survey
fish_spp <- c('15',  '23',  '25',  '26',  '28',  '32',
              '33',  '72',  '73',  '74',  '75',  '76',
              '77',  '78',  '84',  '101', '102', '103',
              '104', '105', '106', '108', '121', '139',
              '141', '155', '156', '163', '164', '192',
              '193', '197', '301', '311', '312', '313',
              '502', '503')
# set which stocks occur in more than one region
multi_stock_fish = c(72,73,74,77,105,106)

survdat <-data.frame(survdat)
bn_setup<-survdat[,c('ID', 'STATUS_CODE', 'EST_YEAR', 'SEASON', 'STRATUM', 'DECDEG_BEGLAT', 'SVSPP', 'CATCHSEX', 'BIOMASS', 'ABUNDANCE')]
# add stratum areas to the data.frame
bn_setup<-merge(bn_setup,stratum.area)
bn_setup<-bn_setup[order(bn_setup$ID),] # order the data by ID

# select years and season(s)
# bn_setup<-bn_setup[bn_setup$SEASON=='SPRING' | bn_setup$SEASON=='FALL',]
bn_setup<-bn_setup[bn_setup$SEASON=='SPRING',]
bn_setup<-bn_setup[bn_setup$EST_YEAR>=1968,]
bn_setup<-bn_setup[bn_setup$EST_YEAR<=2014,]

# Select strata (same as in Nye et al. 2009 and Henderson et al. 2017)
s1 = bn_setup[bn_setup$STRATUM>=01010 & bn_setup$STRATUM<=01300,]
s2 = bn_setup[bn_setup$STRATUM>=01360 & bn_setup$STRATUM<=01400,]
s3 = bn_setup[bn_setup$STRATUM>=01610 & bn_setup$STRATUM<=01760,]
bn_setup<-rbind(s1,s2,s3)

#Find total area in strata of interest and create a strata "weights" column
allstr<-bn_setup[,c(1,11)]
allstr<-allstr[!duplicated(allstr["STRATUM"]),]
totstrwt<-sum(allstr$STRATUM_AREA)
bn_setup$STRATIO<-bn_setup$STRATUM_AREA/totstrwt

#Select species
bn_setup<-bn_setup[bn_setup$SVSPP %in% fish_spp,]
unique(bn_setup$SVSPP)

# Select unique rows to get rid of length duplicates
bn_unique<-bn_setup[!duplicated(bn_setup[c("ID", "SVSPP", "CATCHSEX")]),]

# Add ecoregion column: N=northern, S=southern, NS=single-stock
bn_unique$ECOREGION <- 'NS'
# for multiple stock species, mark ecoregion accordingly.
# make a data.frame for northern stocks (Gulf of Maine)
multi_stock <- bn_unique[bn_unique$SVSPP %in% multi_stock_fish,]
  sort(unique(multi_stock$SVSPP))
  multi_stock_fish
nstrat1 <- multi_stock[multi_stock[,1]==01240,]
nstrat2 <- multi_stock[multi_stock[,1]>=01260 & multi_stock[,1]<=01300,]
nstrat3 <- multi_stock[multi_stock[,1]>=01360 & multi_stock[,1]<=01400,]
north <- rbind(nstrat1, nstrat2, nstrat3)
north$ECOREGION <- 'N'
# make data.frame for southern stocks (Southern New England --> Cape Hatteras)
ind_south <- which(is.na(match(multi_stock$ID, north$ID)))
south <- multi_stock[ind_south,]
south$ECOREGION <- 'S'
# make data.frame for species with just one stock over the whole shelf.
single_stock = bn_unique[!bn_unique$SVSPP %in% multi_stock_fish,]
# combine all 3 data.frames into a single df
bn_unique1 <- rbind(north, south, single_stock)
length(sort(unique(paste0(bn_unique1$SVSPP, bn_unique1$ECOREGION))))  # 44 stocks
head(bn_unique1); tail(bn_unique1)

# Log-transform biomass
bn_unique1[,'BIOMASS']<-log10(bn_unique1[,'BIOMASS']+1)
head(bn_unique1)

#SUM BIOMASSES AND ABUNDANCES OVER CATCHSEX	
bn_unique2<-aggregate(cbind(BIOMASS, ABUNDANCE) ~ ID + SVSPP + ECOREGION, sum, data=bn_unique1)
bn_unique2<-bn_unique2[order(bn_unique2$ID),]
bn_unique3<-unique(bn_unique1[,c('ID', 'STRATUM','EST_YEAR', 'SEASON', 'DECDEG_BEGLAT', 'STRATUM_AREA', 'STRATIO')])
bn_unique4<-merge(bn_unique2, bn_unique3, by="ID")

# count tows by stratum
tows_unique<-bn_unique[!duplicated(bn_unique["ID"]),] 
numtows<-aggregate(ID~EST_YEAR+STRATUM,length,data=tows_unique)
colnames(numtows)<-c('EST_YEAR','STRATUM','COUNT')

# stratified mean biomass in loop
years<-seq(1968,2014)
# specify names for each stock column.
svspp<- c('15_NS','23_NS','25_NS','26_NS','28_NS','32_NS','33_NS','72_N','72_S','73_N','73_S','74_N','74_S','75_NS','76_NS','77_N','77_S','78_NS','84_NS','101_NS','102_NS','103_NS','104_NS','105_N','105_S','106_N', '106_S','108_NS','121_NS','139_NS','141_NS','155_NS','156_NS','163_NS','164_NS','192_NS','193_NS','197_NS','301_NS', '311_NS','312_NS', '313_NS', '502_NS', '503_NS')
stratum<-sort(unique(bn_unique4$STRATUM))
stmean_out<-matrix(0,length(years),length(svspp))
colnames(stmean.B.loop.out)<-as.character(svspp)
rownames(stmean.B.loop.out)<-as.character(years)
bn_setup_strata<-aggregate(BIOMASS~EST_YEAR + SVSPP_ECOREGION + STRATUM + STRATIO, sum, data=bn_unique4)
bn_setup_strata<-merge(bn_setup_strata,numtows)
bn_setup_strata$MBIOM<-bn_setup_strata$BIOMASS/bn_setup_strata$COUNT
bn_setup_strata$WTMEAN<-bn_setup_strata$MBIOM*bn_setup_strata$STRATIO

for(s in svspp){
  for(i in years){
    data<-bn_setup_strata[bn_setup_strata$SVSPP_ECOREGION==s,]
    data<-data[data$EST_YEAR==i,]
    stmean_out[which(years==i),which(svspp==s)]<-sum(data$WTMEAN)
  }
}
head(stmean_out)

write.csv(stmean_out, '...stmean biomass.csv')