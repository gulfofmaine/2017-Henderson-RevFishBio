# building the metrics data frame with stratified mean biomass, center of biomass metrics, and biomass-weighted center of distribution

# cob_data is the MATLAB output
cob_data <- read.table('...COB MATLAB output.txt')

mydata <- read.csv('...COB prep for MATLAB.csv')[-1]
years <- unique(mydata$EST_YEAR)
svspp<- c('15','23','25','26','28','32','33','72_N','72_S','73_N','73_S','74_N','74_S','75','76','77_N','77_S','78','84','101','102','103','104','105_N','105_S','106_N', '106_S','108','121','139','141','155','156','163','164','192','193','197','301', '311', '312', '313', '502', '503')
length(svspp)*length(years)

# stocks
fish_species<-svspp
years<-unique(mydata$EST_YEAR)
year_vec<-rep(years, times=length(fish_species))
species_vector<-c()
for (stock in fish_species){
  species_vector<-c(species_vector, rep(stock, times=length(years)))
}
species_vector<-as.factor(species_vector)

# MAX and MIN LATITUDE
cob_data2<-cbind(YEAR=year_vec, Stock=species_vector, cob_data)
colnames(cob_data2)<-c('YEAR', 'Stock', 'Lat_mean', 'Lon_mean', 'Rot_Lat_mean', 'Rot_Lon_mean', 'Along_mean', 'Cross_mean')
Max_Lat<-c()
Min_Lat<-c()
head(mydata)
ind_start<-14
ind_lat<-12

for (stock in ind_start:dim(mydata)[2]){
  ind_spp<-stock-13
  stock_num<-fish_species[ind_spp]
  print(stock_num)
  
  for (yr in years){
    temp<-mydata[mydata$EST_YEAR==yr, c(ind_lat,stock)]
    temp2<-temp[temp[,2]>0,]
    
    if (dim(temp2)[1]==0){
      Max_Lat<-c(Max_Lat, 'NA')
      Min_Lat<-c(Min_Lat, 'NA')
    }else{
      lat<-temp2[,1]
      Max_Lat<-c(Max_Lat, max(lat))
      Min_Lat<-c(Min_Lat, min(lat))
    }
  }
}

# MEAN DEPTH -- BIOMASS WEIGHTED
mDepth<-c()
ind_depth<-which(colnames(mydata)=='AVGDEPTH')
for (stock in ind_start:dim(mydata)[2]){
  for (yr in years){
    temp<-mydata[mydata$EST_YEAR==yr, c(ind_depth, stock)]
    ind_NA<-which(!is.na(temp[,1]))
    temp2<-temp[ind_NA,]
    depth<-temp2$AVGDEPTH
    weight<-temp2[,2]    
    wD<-sum(weight*depth)/sum(weight)
    mDepth<-c(mDepth, wD)
  }
}

# MEAN BOTTEMP and SURFTEMP -- weighted by biomass
mBT<-c()
mSST<-c()
ind_bottemp<-which(colnames(mydata)=='BOTTEMP')
ind_surftemp<-which(colnames(mydata)=='SURFTEMP')
for (stock in ind_start:dim(mydata)[2]){
  for (yr in years){
    surftemp<-mydata[mydata$EST_YEAR==yr, c(ind_surftemp, stock)]
    bottemp<-mydata[mydata$EST_YEAR==yr, c(ind_bottemp, stock)]
    ind_surfNA<-which(!is.na(surftemp[,1]))
    ind_botNA<-which(!is.na(bottemp[,1]))
    surftemp2<-surftemp[ind_surfNA,]
    bottemp2<-bottemp[ind_botNA,]
    surfweight<-surftemp2[,2]
    botweight<-bottemp2[,2]
    surftemp3<-surftemp2$SURFTEMP
    bottemp3<-bottemp2$BOTTEMP
    wSST<-sum(surfweight*surftemp3)/sum(surfweight)
    wBT<-sum(botweight*bottemp3)/sum(botweight)
    mSST<-c(mSST, wSST)
    mBT<-c(mBT, wBT)
  }
}

# DISTANCE
Distance = rep(999, time=length(year_vec))

# PUT TOGETHER AND RENAME
newdata = cbind(year_vec, species_vector, cob_data, Distance, as.numeric(as.character(Max_Lat)), as.numeric(as.character(Min_Lat)), mDepth, mSST, mBT)
colnames(newdata) = c('YEAR', 'Stock', 'Lat_mean', 'Lon_mean', 'Lat_rotated', 'Lon_rotated', 'along_mean', 'cross_mean', 'Distance', 'Max_Lat', 'Min_Lat', 'mDepth', 'mSST', 'mBT')

# FILLING THE DISTANCE COLUMN
library(Imap)
save_dist<-c()
for (ii in 1:length(newdata$Lat_rotated)){
  dist<-gdist(lon.1=-75, lon.2=newdata$Lon_rotated[ii], lat.1=35, lat.2=newdata$Lat_rotated[ii], units='km')
  save_dist<-c(save_dist, dist)
}
newdata$Distance<-save_dist

# STRATIFIED MEAN BIOMASS -- KATHY MILLS 
# NOT log-transformed
strat_mat<-read.csv("...stmean biomass.csv")
strat_biomass<-c()
for (col in 2:dim(strat_mat)[2]){
  strat_biomass<-c(strat_biomass, strat_mat[,col])  
#   strat_biomass<-c(strat_biomass, strat_mat[,col])  
}
newdata$STRAT_BIOMASS<-strat_biomass

# SAVE
write.csv(newdata, 'COB and biomass metrics.csv')

