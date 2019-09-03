setwd("~/Desktop/天邦数据/BLUP")
library(ggplot2)

###################### Duroc

#### Production traits
duroc_pro<-read.table("Duroc_Hanswine_pro.txt",h=T,sep="\t",fileEncoding="UTF-16")
duroc_pro$BirthDate<-as.Date(duroc_pro$BirthDate,format="%Y/%m/%d")
duroc_pro$MidTestDate<-as.Date(duroc_pro$MidTestDate,format="%Y/%m/%d")
duroc_pro$TermTestDate<-as.Date(duroc_pro$TermTestDate,format="%Y/%m/%d")
summary(duroc_pro)

#       ID                BirthFarm   Origin         BirthDate          Sex      BirthWeight     BirthParity      LitterSize        LeftTeat       RightTeat     
# DDTCZZC14200001:    1   AHCZ:9269   EB5:  206   Min.   :2014-03-06   阉:   3   Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000   Min.   : 3.000  
# DDTCZZC14200002:    1   GXGG:1292   US :10355   1st Qu.:2015-08-15   F :5410   1st Qu.:1.200   1st Qu.:1.000   1st Qu.: 3.000   1st Qu.:6.000   1st Qu.: 6.000  
# DDTCZZC14200003:    1                           Median :2017-04-14   M :5148   Median :1.450   Median :1.000   Median :11.000   Median :6.000   Median : 7.000  
# DDTCZZC14200004:    1                           Mean   :2017-01-06             Mean   :1.378   Mean   :1.842   Mean   : 8.806   Mean   :6.479   Mean   : 6.572  
# DDTCZZC14200005:    1                           3rd Qu.:2018-05-26             3rd Qu.:1.700   3rd Qu.:3.000   3rd Qu.:13.000   3rd Qu.:7.000   3rd Qu.: 7.000  
# DDTCZZC14200006:    1                           Max.   :2019-02-05             Max.   :2.800   Max.   :8.000   Max.   :40.000   Max.   :9.000   Max.   :10.000  
# (Other)        :10555                                                          NA's   :2                                                                        

#       Lineage                  Ancestry             TestFarm     Herd           Pen        MidTestDate          MidTestDays     MidTestWeight     TermTestDate       
#  G-DDTCZZC11A528XT:4321   K-DDTCZZC12E933HT: 811   AHCZ:9269   cf :   2          :5969   Min.   :2014-07-03   Min.   : 97.0   Min.   : 31.00   Min.   :2014-08-22  
#  G-DDTCZZC11B403PT:2172   K-DDTCZZC11B403PT: 796   GXGG:1292   yc1: 545   20401  :  48   1st Qu.:2015-08-09   1st Qu.:117.0   1st Qu.: 60.98   1st Qu.:2016-01-16  
#  G-DDTCZZC11D921IT:1154   K-DDTCZZC14203535: 661               yc2: 747   30303  :  34   Median :2017-01-09   Median :120.0   Median : 67.98   Median :2017-08-18  
#  G-DDTCZZC12B918ET: 974   K-DDTCZZC13020453: 633               yu1:6805   30710  :  34   Mean   :2016-11-02   Mean   :120.4   Mean   : 67.38   Mean   :2017-06-06  
#  G-DDTCZZC11B434HT: 331   K-DDTCZZC13020469: 579               yu2: 998   30306  :  33   3rd Qu.:2017-12-08   3rd Qu.:123.0   3rd Qu.: 74.20   3rd Qu.:2018-11-14  
#  G-DD2513900013801: 239   K-DDTCZZC12B918ET: 487               yu3:1464   30801  :  33   Max.   :2019-06-03   Max.   :250.0   Max.   :103.97   Max.   :2019-05-30  
#  (Other)          :1370   (Other)          :6594                          (Other):4410   NA's   :3345         NA's   :3346    NA's   :3345     NA's   :1307        

#   TermTestDays   TermTestWeight     PostFeed            BF             LMA             LMD        Days60_corr     Days100_corr    Days115_corr     ADG60_corr   
#  Min.   :137.0   Min.   : 78.0   Min.   : 42.73   Min.   : 4.20   Min.   :10.98   Min.   :3.96   Min.   : 91.6   Min.   :132.1   Min.   :134.5   Min.   :285.4  
#  1st Qu.:165.0   1st Qu.:101.9   1st Qu.:103.60   1st Qu.: 8.80   1st Qu.:25.96   1st Qu.:6.07   1st Qu.:106.9   1st Qu.:153.8   1st Qu.:165.0   1st Qu.:489.9  
#  Median :170.0   Median :111.0   Median :116.07   Median :10.00   Median :30.49   Median :6.60   Median :112.3   Median :161.0   Median :174.9   Median :521.9  
#  Mean   :171.4   Mean   :111.1   Mean   :116.54   Mean   :10.31   Mean   :30.68   Mean   :6.56   Mean   :114.4   Mean   :163.0   Mean   :177.8   Mean   :516.7  
#  3rd Qu.:175.0   3rd Qu.:120.0   3rd Qu.:128.85   3rd Qu.:11.94   3rd Qu.:35.39   3rd Qu.:7.06   3rd Qu.:119.8   3rd Qu.:170.1   3rd Qu.:187.6   3rd Qu.:548.2  
#  Max.   :213.0   Max.   :168.0   Max.   :203.63   Max.   :23.88   Max.   :60.15   Max.   :9.27   Max.   :205.3   Max.   :225.7   Max.   :259.4   Max.   :645.0  
#  NA's   :1307    NA's   :1307    NA's   :7182     NA's   :1307    NA's   :1307    NA's   :1307   NA's   :3346    NA's   :1307    NA's   :1307    NA's   :3346   

#   ADG100_corr     ADG115_corr     FCR100_corr     FCR115_corr      BF100_corr       BF115_corr     LMA100_corr     LMA115_corr     LMD100_corr    LMD115_corr  
#  Min.   :439.4   Min.   :440.3   Min.   :0.681   Min.   :0.829   Min.   : 4.400   Min.   : 5.03   Min.   :11.43   Min.   :12.43   Min.   :3.96   Min.   :3.96  
#  1st Qu.:579.6   1st Qu.:605.3   1st Qu.:1.654   1st Qu.:1.801   1st Qu.: 8.100   1st Qu.: 9.24   1st Qu.:23.96   1st Qu.:26.07   1st Qu.:6.07   1st Qu.:6.07  
#  Median :612.4   Median :649.5   Median :1.898   Median :2.046   Median : 9.270   Median :10.57   Median :27.52   Median :29.94   Median :6.60   Median :6.60  
#  Mean   :608.3   Mean   :645.0   Mean   :1.920   Mean   :2.068   Mean   : 9.389   Mean   :10.70   Mean   :28.21   Mean   :30.70   Mean   :6.56   Mean   :6.56  
#  3rd Qu.:640.8   3rd Qu.:688.2   3rd Qu.:2.165   3rd Qu.:2.313   3rd Qu.:10.510   3rd Qu.:11.97   3rd Qu.:32.30   3rd Qu.:35.15   3rd Qu.:7.06   3rd Qu.:7.06  
#  Max.   :757.3   Max.   :854.8   Max.   :4.255   Max.   :4.403   Max.   :22.300   Max.   :25.76   Max.   :47.96   Max.   :52.18   Max.   :9.27   Max.   :9.27  
#  NA's   :1307    NA's   :1307    NA's   :7204    NA's   :7204    NA's   :1307     NA's   :1307    NA's   :9489    NA's   :9489    NA's   :1307   NA's   :1307  

duroc_pro<-droplevels(duroc_pro[duroc_pro$Sex!="阉",]) # remove undetermined sex
duroc_pro<-droplevels(duroc_pro[duroc_pro$Herd!="cf",]) # remove records from delivery room
nrow(duroc_pro) # 10556

######## Density distribution of production traits
BirthWeight<-droplevels(duroc_pro[duroc_pro$BirthWeight!=0,c("Sex","TestFarm","BirthWeight")])
BirthWeight$Trait<-"BirthWeight"
colnames(BirthWeight)[3]<-"Value"

Days60_corr<-duroc_pro[,c("Sex","TestFarm","Days60_corr")]
Days60_corr$Trait<-"Days60_corr"
colnames(Days60_corr)[3]<-"Value"

Days100_corr<-duroc_pro[,c("Sex","TestFarm","Days100_corr")]
Days100_corr$Trait<-"Days100_corr"
colnames(Days100_corr)[3]<-"Value"

Days115_corr<-duroc_pro[,c("Sex","TestFarm","Days115_corr")]
Days115_corr$Trait<-"Days115_corr"
colnames(Days115_corr)[3]<-"Value"

ADG60_corr<-duroc_pro[,c("Sex","TestFarm","ADG60_corr")]
ADG60_corr$Trait<-"ADG60_corr"
colnames(ADG60_corr)[3]<-"Value"

ADG100_corr<-duroc_pro[,c("Sex","TestFarm","ADG100_corr")]
ADG100_corr$Trait<-"ADG100_corr"
colnames(ADG100_corr)[3]<-"Value"

ADG115_corr<-duroc_pro[,c("Sex","TestFarm","ADG115_corr")]
ADG115_corr$Trait<-"ADG115_corr"
colnames(ADG115_corr)[3]<-"Value"

FCR100_corr<-duroc_pro[,c("Sex","TestFarm","FCR100_corr")]
FCR100_corr$Trait<-"FCR100_corr"
colnames(FCR100_corr)[3]<-"Value"

FCR115_corr<-duroc_pro[,c("Sex","TestFarm","FCR115_corr")]
FCR115_corr$Trait<-"FCR115_corr"
colnames(FCR115_corr)[3]<-"Value"

BF100_corr<-duroc_pro[,c("Sex","TestFarm","BF100_corr")]
BF100_corr$Trait<-"BF100_corr"
colnames(BF100_corr)[3]<-"Value"

BF115_corr<-duroc_pro[,c("Sex","TestFarm","BF115_corr")]
BF115_corr$Trait<-"BF115_corr"
colnames(BF115_corr)[3]<-"Value"

LMA100_corr<-duroc_pro[,c("Sex","TestFarm","LMA100_corr")]
LMA100_corr$Trait<-"LMA100_corr"
colnames(LMA100_corr)[3]<-"Value"

LMA115_corr<-duroc_pro[,c("Sex","TestFarm","LMA115_corr")]
LMA115_corr$Trait<-"LMA115_corr"
colnames(LMA115_corr)[3]<-"Value"

LMD100_corr<-duroc_pro[,c("Sex","TestFarm","LMD100_corr")]
LMD100_corr$Trait<-"LMD100_corr"
colnames(LMD100_corr)[3]<-"Value"

df<-rbind(BirthWeight,Days60_corr,Days100_corr,Days115_corr,
          ADG60_corr,ADG100_corr,ADG115_corr,FCR100_corr,
          FCR115_corr,BF100_corr,BF115_corr,
          LMA100_corr,LMA115_corr,LMD100_corr)

df$Trait<-factor(df$Trait,levels=c("BirthWeight","Days60_corr","Days100_corr","Days115_corr",
                                   "ADG60_corr","ADG100_corr","ADG115_corr","BF100_corr","BF115_corr",
                                   "LMA100_corr","LMA115_corr","LMD100_corr","FCR100_corr",
                                   "FCR115_corr"))

## total
tiff(file="total_density_duroc_pro.tif",width=8, height=12,units='in',family="Times New Roman",res=300)

ggplot(df, aes(x=Value)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + facet_wrap( ~ Trait, ncol=2, scales="free")

dev.off()

## ~ Sex
tiff(file="Sex_density_duroc_pro.tif",width=8, height=12,units='in',family="Times New Roman",res=300)
ggplot(df, aes(x=Value, color=Sex, fill=Sex)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) + facet_wrap( ~ Trait, ncol=2, scales="free")
dev.off()

## ~ Test Farm
tiff(file="TestFarm_density_duroc_pro.tif",width=8, height=12,units='in',family="Times New Roman",res=300)
ggplot(df, aes(x=Value, color=TestFarm, fill=TestFarm)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+facet_wrap( ~ Trait, ncol=2, scales="free")+
  geom_density(alpha=.2)
dev.off()

#### mid-test days handling
# time_df<-floor(mean(duroc_pro$TermTestDate-duroc_pro$MidTestDate,na.rm=T)) # 48.9399 days bewteen terminal test day and mid-test day
# sqrt(var(duroc_pro$TermTestDate-duroc_pro$MidTestDate,na.rm=T)) # 4.647253
# 
# for(i in 1:nrow(duroc_pro)){
#   if(is.na(duroc_pro[i,"MidTestDate"]) & is.na(duroc_pro[i,"TermTestDate"])){
#     duroc_pro[i,"MidTestDate"]<-NA
#   }
#   if(is.na(duroc_pro[i,"MidTestDate"]) & !is.na(duroc_pro[i,"TermTestDate"])){
#     duroc_pro[i,"MidTestDate"]<- duroc_pro[i,"TermTestDate"] - time_df
#   }
# }

#### pedigree and phenotype data for DMU
library(pedigree)

levels(duroc_pro$TestFarm) # [1] "AHCZ" "GXGG"
duroc_pro$TestFarm<-as.numeric(duroc_pro$TestFarm)
levels(duroc_pro$Sex) # [1] "F" "M"
duroc_pro$Sex<-as.numeric(duroc_pro$Sex)
levels(duroc_pro$Herd) # [1] "yc1" "yc2" "yu1" "yu2" "yu3"
duroc_pro$Herd<-as.numeric(duroc_pro$Herd)

ped_ori<-read.table("Duroc_pedigree_origin.txt",h=T,sep="\t",fileEncoding="UTF-16",colClasses = "character")
ind<-c(ped_ori$个体号,ped_ori$父亲,ped_ori$母亲,ped_ori$父父,ped_ori$父母,ped_ori$母父,ped_ori$母母)
sire<-c(ped_ori$父亲,ped_ori$父父,ped_ori$母父,ped_ori$父父父,ped_ori$父母父,ped_ori$母父父,ped_ori$母母父)
dam<-c(ped_ori$母亲,ped_ori$父母,ped_ori$母母,ped_ori$父父母,ped_ori$父母母,ped_ori$母父母,ped_ori$母母母)
ped<-cbind(ind,sire,dam)
#ped<-ped[which(ped[,1]!=""),]
ped<-unique(ped[,1:3])
ped[ped==""]<-NA
#write.table(ped,"ped_temp.txt",sep="\t",col.names=F,row.names=F,quote=F)

ped<-add.Inds(ped)
ord <- orderPed(ped)
ped<-ped[order(ord),]
#ped<-ped[-1,]
ord2<-0:(nrow(ped)-1)
ped<-cbind(ped,ord2)
ped[,1]<-as.character(ped[,1])
ped[,2]<-as.character(ped[,2])
ped[,3]<-as.character(ped[,3])

ped[is.na(ped)]<-0

duroc_pro[!duroc_pro[,1]%in%ped[,1],1] # [1] DDTCZZC15401117 DDTCZZC15401115 two individuals with phenotype data were not involved in the pedigree due to 
# the error in their pedigree, so they were removed.
duroc_pro<-droplevels(duroc_pro[duroc_pro[,1]%in%ped[,1],])
parity<-duroc_pro$BirthParity
parity[parity>=2]<-2
parity[parity==0]<--999
sex=duroc_pro$Sex

#### herd year season
getSeason <- function(DATE) {
  if(is.na(DATE)){
    return(NA)
  }else{
    month <- as.numeric(format(DATE,'%m'))
    if (month >= 12 | month <= 2){return(4)}
    if (month >= 3 & month <= 5){return(1)}
    if (month >= 6 & month <= 8){return(2)}
    if (month >= 9 & month <= 11){return(3)}
  }
}

hys<-rep(NA,nrow(duroc_pro))

for(i in 1:nrow(duroc_pro)){
  if(!is.na(duroc_pro[i,"BirthDate"])){
    date<-duroc_pro[i,"BirthDate"]
    year<-substr(format(date,'%Y'),4,4)
    season<-getSeason(date)
    hys[i]<-paste0(as.numeric(duroc_pro[i,"Herd"]),year,season)
  }
}
head(hys)


phe_ind<-ped[match(duroc_pro[,1],ped[,1]),4]
ped_ind<-ped$ord2
sire_ind<-ped[match(ped[,2],ped[,1]),4]
dam_ind<-ped[match(ped[,3],ped[,1]),4]
ped_dmu<-data.frame(ped_ind[-1],sire_ind[-1],dam_ind[-1],ord2[-1])

## trait
BirthWeight<-duroc_pro$BirthWeight
BirthWeight[BirthWeight==0]<--999
BirthWeight[is.na(BirthWeight)]<--999
Days60_corr<-duroc_pro$Days60_corr
Days60_corr[is.na(Days60_corr)]<--999
Days100_corr<-duroc_pro$Days100_corr
Days100_corr[is.na(Days100_corr)]<--999
Days115_corr<-duroc_pro$Days115_corr
Days115_corr[is.na(Days115_corr)]<--999
ADG60_corr<-duroc_pro$ADG60_corr
ADG60_corr[is.na(ADG60_corr)]<--999
ADG100_corr<-duroc_pro$ADG100_corr
ADG100_corr[is.na(ADG100_corr)]<--999
ADG115_corr<-duroc_pro$ADG115_corr
ADG115_corr[is.na(ADG115_corr)]<--999
FCR100_corr<-duroc_pro$FCR100_corr
FCR100_corr[is.na(FCR100_corr)]<--999
FCR115_corr<-duroc_pro$FCR115_corr
FCR115_corr[is.na(FCR115_corr)]<--999
BF100_corr<-duroc_pro$BF100_corr
BF100_corr[is.na(BF100_corr)]<--999
BF115_corr<-duroc_pro$BF115_corr
BF115_corr[is.na(BF115_corr)]<--999
LMA100_corr<-duroc_pro$LMA100_corr
LMA100_corr[is.na(LMA100_corr)]<--999
LMA115_corr<-duroc_pro$LMA115_corr
LMA115_corr[is.na(LMA115_corr)]<--999
LMD100_corr<-duroc_pro$LMD100_corr
LMD100_corr[is.na(LMD100_corr)]<--999


dat_dmu<-data.frame(ID=phe_ind,Farm=duroc_pro$TestFarm,Parity=parity,
                    Sex=sex,Hys=hys,BirthWeight=BirthWeight,
                    Days60_corr=Days60_corr,Days100_corr=Days100_corr,
                    Days115_corr=Days115_corr,ADG60_corr=ADG60_corr,
                    ADG100_corr=ADG100_corr,ADG115_corr=ADG115_corr,
                    BF100_corr=BF100_corr,BF115_corr=BF115_corr,
                    LMA100_corr=LMA100_corr,LMA115_corr=LMA115_corr,
                    LMD100_corr=LMD100_corr,FCR100_corr=FCR100_corr,
                    FCR115_corr=FCR115_corr)

write.table(ped_dmu,"DMU_PED_DUROC",col.names=F,row.names=F,quote=F)
write.table(dat_dmu,"DMU_DAT_DUROC_PRO",col.names=F,row.names=F,quote=F)
cat(paste0(paste(colnames(dat_dmu),collapse="\t"),"\n"),file="DMU_DAT_DUROC_PRO_HEADER")


## family tree
makeA(ped, which=rep(TRUE, nrow(ped)))
a <- read.table("A.txt",header=F)
nI <- as.integer(nrow(ped))
a_mat <- as(new("dsTMatrix",
                Dim=c(nI, nI),
                uplo="L",
                i=(as.integer(a$V1)-1L),
                j=(as.integer(a$V2)-1L),
                x=a$V3),
            "dsCMatrix")
a_mat<- as.matrix(a_mat)
plot(im(a_mat[nrow(a_mat):1,]),main="Genomic relationship Matrix",axes=F)

d<-as.dist(a_mat)
hc<-hclust(1-d)
dend<-as.dendrogram(hc)
plot(dend,ylim=c(0,1.0))
