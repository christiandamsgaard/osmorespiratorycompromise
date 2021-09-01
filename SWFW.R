require(lubridate)
require(readxl)
require(ggplot2)
require(ape)
require(phytools)
require(nlme)
require(geiger)
require(ggsignif)
require(scales)
require(cowplot)
require(scales)



#########################
### GILL SURFACE AREA ###
#########################

setwd("/Users/au231308/Dropbox/Projects/SWFW/")

# Import data
df<-read_xlsx("./data.xlsx",sheet = "GSA")
df<-as.data.frame(df)

df<-subset(x = df,select = c("Species","Salinity","Mass (g)","SA (cm^2)","SA/g (cm2/g)"))
colnames(df)<-c("sp","sal","bm","sa","sag") # abbreviated column names
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate genus and species by underscore
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate species and subspecies by underscore



# Plot raw mass-specific metabolic rate without correcting for allometric scaling and phylogenetic non-independence
df.0<-
  cbind(
    aggregate(sag~sp,df,mean),
    aggregate(sal~sp,df,unique)[,2]
  )

colnames(df.0)[3]<-"sal"

p.value<-t.test(sag~sal,df.0)$p.value
p.value


p0<-
  ggplot(
    data = df.0,
    mapping = aes(x=sal,y=sag,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("Mass-specific gill surface area (mm"^"2"*" g"^"-1"*")"))+
  ylim(c(
    min(df.0$sag),
    1.2*max(df.0$sag)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*df.0$sag), vjust=0,annotation=ifelse(test = p.value<0.001, no = paste("P = ",signif(p.value,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black");p0


df<-df[!is.na(df$bm),] # remove studies that do not report body mass

# in species that do not report total gill surface area, calculate total surface area from body mass and mass specific surface area
for (i in which(is.na(df$sa))){
  df$sa[i]<-df$sag[i]*df$bm[i]
}

# Import tree
tree<-read.tree(file = "./mcc.nexus")

# Correct recent changes in species names
df$sp[which(df$sp=="Ambloplites_rupestri")]<-"Ambloplites_rupestris"
df$sp[which(df$sp=="Amerurus_nebulosis")]<-"Ameiurus_nebulosus"
df$sp[which(df$sp=="Anguilla_vulgaris")]<-"Anguilla_anguilla"
df$sp[which(df$sp=="Botia_ohachata")]<-"Botia_lohachata"
df$sp[which(df$sp=="Catostoumus_commersonii")]<-"Catostomus_commersonii"
df$sp[which(df$sp=="Channa_punctatus")]<-"Channa_punctata"
df$sp[which(df$sp=="Clarias_mossambicus")]<-"Clarias_gariepinus"
df$sp[which(df$sp=="Comephorus_baicalensis")]<-"Comephorus_baikalensis"
df$sp[which(df$sp=="Gnathonemus_victoriae")]<-"Marcusenius_victoriae"
df$sp[which(df$sp=="Hypostomus_plecostomus.")]<-"Hypostomus_plecostomus"
df$sp[which(df$sp=="Ictalurus_nebulosus")]<-"Ameiurus_nebulosus"
df$sp[which(df$sp=="Noemacheilus_barbatulus")]<-"Barbatula_barbatula"
df$sp[which(df$sp=="Notropis_cornutus)")]<-"Luxilus_cornutus"
df$sp[which(df$sp=="Oncorynchus_mykiss")]<-"Oncorhynchus_mykiss"
df$sp[which(df$sp=="Oncorhynchus_mykis")]<-"Oncorhynchus_mykiss"
df$sp[which(df$sp=="Oreochromis_alcalicus_grahami")]<-"Alcolapia_grahami"
df$sp[which(df$sp=="Prognathochromis_venator")]<-"Haplochromis_venator"
df$sp[which(df$sp=="Stizostedion_vitreum")]<-"Sander_vitreus"
df$sp[which(df$sp=="Acanthocottus_scorpius")]<-"Myoxocephalus_scorpius"
df$sp[which(df$sp=="Anoplomoma_fimbria")]<-"Anoplopoma_fimbria"
df$sp[which(df$sp=="Archosargus_probaiocephalus")]<-"Archosargus_probatocephalus"
df$sp[which(df$sp=="Centropristis_striatus")]<-"Centropristis_striata"
df$sp[which(df$sp=="Chilomycterus_schoepfi")]<-"Chilomycterus_schoepfii"
df$sp[which(df$sp=="Clinottus_globiceps")]<-"Clinocottus_globiceps"
df$sp[which(df$sp=="Dicentrarchuslabrax")]<-"Dicentrarchus_labrax"
df$sp[which(df$sp=="Errex_zachirus")]<-"Glyptocephalus_zachirus"
df$sp[which(df$sp=="Gadus_virens")]<-"Pollachius_virens"
df$sp[which(df$sp=="Gymnosarda_alleterata")]<-"Euthynnus_alletteratus"
df$sp[which(df$sp=="Leptocephalus_conger")]<-"Conger_conger"
df$sp[which(df$sp=="Lophopsetta_maculata")]<-"Mancopsetta_maculata"
df$sp[which(df$sp=="Palinurichthyes_perciformis")]<-"Hyperoglyphe_perciformis"
df$sp[which(df$sp=="Peprilus_alepidatus")]<-"Peprilus_paru"
df$sp[which(df$sp=="Poecilialatipinna")]<-"Poecilia_latipinna"
df$sp[which(df$sp=="Pomatomus_saliatrix")]<-"Pomatomus_saltatrix"
df$sp[which(df$sp=="Poronotus_triacanthus")]<-"Peprilus_triacanthus"
df$sp[which(df$sp=="Prionotus_strigatus")]<-"Prionotus_evolans"
df$sp[which(df$sp=="Roccus_lineatus")]<-"Morone_saxatilis"
df$sp[which(df$sp=="Sarda_chiliensis")]<-"Sarda_chiliensis_chiliensis"
df$sp[which(df$sp=="Spheroides_maculatus")]<-"Sphoeroides_maculatus"
df$sp[which(df$sp=="Tautoga_onitus")]<-"Tautoga_onitis"

# Has any species changed name?
df$sp[is.na(match(df$sp,tree$tip.label))]

# Prune tree to only represent species in the data set
tree<-keep.tip(tree,df$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)

# Set up glmm model
prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
tree$node.label<-seq(1,length(tree$node.label),1)
inv.phylo<-MCMCglmm::inverseA(tree,nodes="TIPS",scale=TRUE)

glmm.sa.0<-
  MCMCglmm::MCMCglmm(
    log10(sa)~log10(bm),
    random=~sp,
    family="gaussian",
    ginverse=list(sp=inv.phylo$Ainv),
    prior=prior,
    data=df,
    nitt=1000000,
    burnin=50000,
    thin=1000)
summary(glmm.sa.0)

# extract intercept and slope
int<-summary(glmm.sa.0)$solutions[1,1];int
slo<-summary(glmm.sa.0)$solutions[2,1];slo
summary(glmm.sa.0)$solutions

# Calculate residuals
df$resid<-log10(df$sa)-(log10(df$bm)*slo+int)

# Genereate named vector for the residuals
resid<-aggregate(resid~sp,df,median)
resid<-setNames(resid$resid,resid$sp)
resid.mo2<-resid
resid


# Genereate named vector for salinity
sal<-aggregate(sal~sp,df,unique)
sal<-setNames(sal$sal,sal$sp)
sal

#phyloaov
paov<-phylANOVA(tree = tree,sal,resid,nsim = 50000)
phylosig(tree = tree,x = resid,method = "lambda",test = T)


# Set colors
col = to.matrix(sal[tree$tip.label],seq=sort(unique(sal)))[,1]
col[which(col == 0)]<-"#08519C"
col[which(col == 1)]<-"#006D2C"
col<-col[tree$tip.label]


# Set up axis text format
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# Plot gill surface area vs body mass scatter plot
p1<-ggplot(data = df, mapping = aes(x = bm, y = sa, col = sal))+
  scale_x_continuous(trans = "log10",
                     limits = c(1,200000),
                     labels = comma
                     )+
  scale_y_continuous(trans = "log10",labels = comma)+
  annotate(size = 6/3,geom = "text",
           label = ifelse(test = summary(glmm.sa.0)$solutions[2,5]<0.001, no = paste("P = ",signif(summary(glmm.sa.0)$solutions[2,5],3),sep=""), yes = paste("P < 0.001")),
           x=5,y=100000)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.75, 0.15),
    legend.spacing.y = unit(0.01,"cm"),
    legend.spacing.x = unit(0.001,"cm"),
    legend.key.height = unit(0.01,"cm"),
    legend.background = element_rect(fill=alpha('blue', 0))
    )+
  geom_abline(slope = slo,intercept = int)+
  geom_point(shape = 16, size = 1)+
  scale_color_manual(values = c("#006D2C","#08519C"),breaks = c("FW","SW"),labels = c("Freshwater","Seawater"))+
  labs(col = "Water",
       x = expression("Body mass (g)"), 
       y = expression("Gill surface area (mm"^"2"*")"));p1



## Jitter plot of residuals
p2<-
  ggplot(
    data = data.frame(
      resid = resid,
      sal = sal
    ),
    mapping = aes(x=sal,y=resid,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("residual log"[10]*"[gill surface area (mm"^"2"*")]"))+
  ylim(c(
    min(data.frame(resid = resid,sal = sal)$resid),
    1.2*max(data.frame(resid = resid,sal = sal)$resid)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*resid), vjust=0,annotation=ifelse(test = paov$Pf<0.001, no = paste("P = ",signif(paov$Pf,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black");p2



## Model estimate of gill surface area
# specify body mass in g
bodymass <- 100

# absolute gill surface area
10^(log10(bodymass)*slo+int)

# mass-specific gill surface area
10^(log10(bodymass)*slo+int)/bodymass


##################################
### HEMOGLOBIN OXYGEN AFFINITY ###
##################################

# Import data
df<-read_xlsx("./data.xlsx",sheet = "P50")
df<-as.data.frame(df)

df<-subset(x = df,select = c("Species","Salinity","P50 (mmHg)","Temp"))
colnames(df)<-c("sp","sal","p50","temp") # abbreviated column names
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate genus and species by underscore
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate species and subspecies by underscore



# Import tree
tree<-read.tree(file = "./mcc.nexus")

# Correct recent changes in species names
df$sp[which(df$sp=="Aguilla_rostrata")]<-"Anguilla_rostrata"
df$sp[which(df$sp=="Ambloplites_rupestri")]<-"Ambloplites_rupestris"
df$sp[which(df$sp=="Salmo_gairdneri")]<-"Oncorhynchus_mykiss"
df$sp[which(df$sp=="Arius_leptaspis")]<-"Neoarius_leptaspis"
df$sp[which(df$sp=="Fundulus_heteroclitus")]<-"Fundulus_heteroclitus_heteroclitus"
df$sp[which(df$sp=="Pangasius")]<-"Pangasianodon_hypophthalmus"
df$sp[which(df$sp=="Salmo_trutta_fario")]<-"Salmo_trutta"
df$sp[which(df$sp=="Euthynnus_affinish")]<-"Euthynnus_affinis"
df$sp[which(df$sp=="F._malcomi")]<-"Forsterygion_malcolmi"
df$sp[which(df$sp=="F._varium")]<-"Forsterygion_varium"
df$sp[which(df$sp=="G._capito")]<-"Forsterygion_capito"
df$sp[which(df$sp=="Gadus_morhua;")]<-"Gadus_morhua"
df$sp[which(df$sp=="Pleuronectes_platessa;")]<-"Pleuronectes_platessa"
df$sp[which(df$sp=="Scomber_japonicus)")]<-"Scomber_japonicus"
df$sp[which(df$sp=="Leucoraja_ocellata)")]<-"Scomber_japonicus"
df$sp[which(df$sp=="Helcogramma_medium")]<-"Bellapiscis_medius"


# I have removed these species, as they are not teleosts
df <- df[which(df$sp!="Potamotrygon_motoro"),] 
df <- df[which(df$sp!="Leucoraja_ocellata"),] 
df <- df[which(df$sp!="Myliobatis_californica"),] 

# I chose a random species from these genera
df$sp[which(df$sp=="Hypostomus_sp")]<-"Hypostomus_agna" 

# Has any species changed name?
df$sp[is.na(match(df$sp,tree$tip.label))]

df<-df[!is.na(df$temp),] # remove studies that do not report temperature


# Thickness without phylogenetic correction
df.0.p50<-
  cbind(
    aggregate(p50~sp,df,mean),
    aggregate(sal~sp,df,unique)[,2]
  )


colnames(df.0.p50)[3]<-"sal"

p.value.p50<-t.test(p50~sal,df.0.p50)$p.value
p.value.p50


p0.p50<-
  ggplot(
    data = df.0.p50,
    mapping = aes(x=sal,y=p50,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("Hb P"[50]*" (mmHg)"))+
  ylim(c(
    min(df.0.p50$p50),
    1.2*max(df.0.p50$p50)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*df.0.p50$p50), vjust=0,annotation=ifelse(test = p.value.p50<0.001, no = paste("P = ",signif(p.value.p50,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black");p0.p50


# Temperature correction
df$inv.temp<-1/(273+df$temp) # calculate 1/(abs Temp)

# Prune tree to only represent species in the data set
tree<-keep.tip(tree,df$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)


## Does P50 depend on temperature?


# Set up glmm model
prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
tree$node.label<-seq(1,length(tree$node.label),1)
inv.phylo<-MCMCglmm::inverseA(tree,nodes="TIPS",scale=TRUE)

glmm<-
  MCMCglmm::MCMCglmm(
    log10(p50)~inv.temp,
    random=~sp,
    family="gaussian",
    ginverse=list(sp=inv.phylo$Ainv),
    prior=prior,
    data=df,
    nitt=1000000,
    burnin=50000,
    thin=1000)
summary(glmm) 

# Extract solutions
int<-summary(glmm)$solutions[1,1];int
slo<-summary(glmm)$solutions[2,1];slo

# Calculate residuals
df$resid<-log10(df$p50)-(df$inv.temp*slo+int)


# Plot data
p3<-ggplot(data = df, mapping = aes(x = inv.temp, y = p50, col = sal))+
    scale_x_continuous(breaks = c(0.0033,0.0035,0.0037),limits = c(0.0032,0.0038),
    sec.axis = sec_axis(
      trans = ~ .^-1 - 273.15,
      name = "Temperature (°C)"))+
  scale_y_continuous(trans = "log10",labels = comma)+
  annotate(size = 6/3,geom = "text",
           label = ifelse(test = summary(glmm)$solutions[2,5]<0.001, no = paste("P = ",signif(summary(glmm)$solutions[2,5],3),sep=""), yes = paste("P < 0.001")),
           x=0.00328,y=60)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.position = "none"
    )+
  geom_abline(slope = slo, intercept = int)+
  geom_point(shape = 16, size = 1)+
  scale_color_manual(values = c("#006D2C","#08519C"),breaks = c("FW","SW"),labels = c("Freshwater","Seawater"))+
  labs(col = "Water",
       x = expression("1/T"), 
       y = expression("Hb P"[50]*" (mmHg)"));p3


# Genereate named vector for p50-residuals
resid<-aggregate(resid~sp,df,median)
resid<-setNames(resid$resid,resid$sp)
resid.p50<-resid
resid

# Genereate named vector for salinity
sal<-aggregate(sal~sp,df,unique)
sal<-setNames(sal$sal,sal$sp)
sal

#phyloaov
paov<-phylANOVA(tree = tree,sal,resid,nsim = 50000)
paov
phylosig(tree = tree,x = resid,method = "lambda",test = T)


# Set colors
col = to.matrix(sal[tree$tip.label],seq=sort(unique(sal)))[,1]
col[which(col == 0)]<-"#08519C"
col[which(col == 1)]<-"#006D2C"
col<-col[tree$tip.label]


## Jitter plot of P50-residuals

p4<-
  ggplot(
    data = data.frame(
      resid = resid,
      sal = sal
    ),
    mapping = aes(x=sal,y=resid,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("residual log"[10]*"[Hb P"[50]*" (mmHg)]"))+
  ylim(c(
    min(data.frame(resid = resid,sal = sal)$resid),
    1.2*max(data.frame(resid = resid,sal = sal)$resid)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*resid), vjust=0,annotation=ifelse(test = paov$Pf<0.001, no = paste("P = ",signif(paov$Pf,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black") ;p4



# Model estimates for p50
FWSW.logP50.effects<-
  aggregate(
    resid~sal,
    data.frame(resid = resid,sal = sal),
    mean)

temperature = 10

# logP50 and P50 without correcting for FW/SW
1/(273.15+temperature)*slo+int
10^(1/(273.15+temperature)*slo+int)


# logP50 and P50 in FW
1/(273.15+temperature)*slo+int+FWSW.logP50.effects[1,2]
10^(1/(273.15+temperature)*slo+int+FWSW.logP50.effects[1,2])

# logP50 and P50 in SW
1/(273.15+temperature)*slo+int+FWSW.logP50.effects[2,2]
10^(1/(273.15+temperature)*slo+int+FWSW.logP50.effects[2,2])


###############
### MO2 MAX ###
###############

# Import data
df<-read_xlsx("./data.xlsx",sheet = "MO2max")
df<-as.data.frame(df)

df<-subset(x = df,select = c("Species","Salinity","Body mass (g)","Uncorrected MO2max (mg O2/h)","MO2max (umol O2/h*g)","MO2 temp"))
colnames(df)<-c("sp","sal","bm","mo2","mo2g","temp") # abbreviated column names
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate genus and species by underscore
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate species and subspecies by underscore

#convert from mg to µmol
df$mo2<-df$mo2/32*1000

df<-df[!is.na(df$bm),] # remove studies that do not report body mass
df

# in species that do not report total gill surface area, calculate total surface area from body mass and mass specific surface area
for (i in which(is.na(df$mo2))){
  df$mo2[i]<-df$mo2g[i]*df$bm[i]
}

# Import tree
tree<-read.tree(file = "./mcc.nexus")

# Correct recent changes in species names
df$sp[which(df$sp=="Ambloplites_rupestri")]<-"Ambloplites_rupestris"
df$sp[which(df$sp=="Ictalurus_nebulosus")]<-"Ameiurus_nebulosus"
df$sp[which(df$sp=="Aristichthys_nobilis")]<-"Hypophthalmichthys_nobilis"
df$sp[which(df$sp=="Gasterosteus_aculeautus")]<-"Gasterosteus_aculeatus"
df$sp[which(df$sp=="Hypophthalmichthys_sp")]<-"Hypophthalmichthys_molitrix"
df$sp[which(df$sp=="Maccullochella_peelii_peelii")]<-"Maccullochella_peelii"
df$sp[which(df$sp=="Onychostoma_sp")]<-"Onychostoma_barbatulum"
df$sp[which(df$sp=="Pelteobagrus_vachelli")]<-"Pseudobagrus_vachellii"
df$sp[which(df$sp=="Salmo_gairdneri")]<-"Oncorhynchus_mykiss"
df$sp[which(df$sp=="Salvelinus_alpinus")]<-"Salvelinus_alpinus_alpinus"
df$sp[which(df$sp=="Chromis_veridis")]<-"Chromis_viridis"
df$sp[which(df$sp=="Leiostomus_xanthrus")]<-"Leiostomus_xanthurus"
df$sp[which(df$sp=="Macrozoraces_americanus")]<-"Zoarces_americanus"
df$sp[which(df$sp=="Neopomacentrus_benkieri")]<-"Neopomacentrus_bankieri"
df$sp[which(df$sp=="Pagrus_auratus")]<-"Sparus_aurata"
df$sp[which(df$sp=="Pomacentrus_lipedogenys")]<-"Pomacentrus_lepidogenys"
df$sp[which(df$sp=="Fundulus_heteroclitus")]<-"Fundulus_heteroclitus_heteroclitus"

# Has any species changed name?
df$sp[is.na(match(df$sp,tree$tip.label))]

# Prune tree to only represent species in the data set
tree<-keep.tip(tree,df$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)

# Set up glmm model
prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
tree$node.label<-seq(1,length(tree$node.label),1)
inv.phylo<-MCMCglmm::inverseA(tree,nodes="TIPS",scale=TRUE)


# Model selection

# Step 1: most complex model
# 1.1: BM and TEMP
glmm1.1<-
  MCMCglmm::MCMCglmm(
    log10(mo2)~log10(bm)+temp,
    random=~sp,
    family="gaussian",
    ginverse=list(sp=inv.phylo$Ainv),
    prior=prior,
    data=df,
    nitt=1000000,
    burnin=100000,
    thin=10000)


# Step 2: remove one variable
# Remove temp
#glmm2.1<-MCMCglmm::MCMCglmm(log10(mo2)~log10(bm),random=~sp,family="gaussian",ginverse=list(sp=inv.phylo$Ainv),prior=prior,data=df,nitt=1000000,burnin=50000,thin=1000)

# Remove temp
#glmm2.2<-MCMCglmm::MCMCglmm(log10(mo2)~temp,random=~sp,family="gaussian",ginverse=list(sp=inv.phylo$Ainv),prior=prior,data=df,nitt=1000000,burnin=50000,thin=1000)

summary(glmm1.1) # DIC = -95
summary(glmm2.1) # DIC = -77 - model 1.1 is better
summary(glmm2.2) # DIC = 230 - model 1.1 is better

# Conclusion on model selection: go ahead with model 1.1


# extract intercept and slope
summary(glmm1.1)

int<-summary(glmm1.1)$solutions[1,1];int
slo1<-summary(glmm1.1)$solutions[2,1];slo1
slo2<-summary(glmm1.1)$solutions[3,1];slo2

summary(glmm1.1)$solutions

# Calculate residuals
df$resid<-log10(df$mo2)-(log10(df$bm)*slo1+df$temp*slo2+int)

# Genereate named vector for the residuals
resid<-aggregate(resid~sp,df,median)
resid<-setNames(resid$resid,resid$sp)
resid.mo2<-resid
resid

# Genereate named vector for salinity
sal<-aggregate(sal~sp,df,unique)
sal<-setNames(sal$sal,sal$sp)
sal

#phyloaov
paov<-phylANOVA(tree = tree,sal,resid,nsim = 50000)
paov
phylosig(tree = tree,x = resid,method = "lambda",test = T)


# Set colors
col = to.matrix(sal[tree$tip.label],seq=sort(unique(sal)))[,1]
col[which(col == 0)]<-"#08519C"
col[which(col == 1)]<-"#006D2C"
col<-col[tree$tip.label]

# Set up axis text format
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# Plot bm and mo2max scatter plot
p5<-ggplot(data = df, mapping = aes(x = bm, y = mo2, col = sal))+
  scale_x_continuous(trans = "log10",
                     limits = c(1,120000),
                     labels = comma)+
  scale_y_continuous(trans = "log10",labels = comma)+
  #geom_abline(slope = slo1, intercept = int)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.8, 0.10),
    legend.spacing.y = unit(0.01,"cm"),
    legend.spacing.x = unit(0.001,"cm"),
    legend.key.height = unit(0.01,"cm"),
    legend.background = element_rect(fill=alpha('blue', 0)))+
  annotate(size = 6/3,geom = "text",
           label = ifelse(test = summary(glmm1.1)$solutions[2,5]<0.01, no = paste("Mass: P = ",signif(summary(glmm1.1)$solutions[2,5],2),sep=""), yes = paste("Mass: P < 0.01")),
           x=6,y=100000)+
  annotate(size = 6/3,geom = "text",
           label = ifelse(test = summary(glmm1.1)$solutions[3,5]<0.01, no = paste("Temp: P = ",signif(summary(glmm1.1)$solutions[3,5],2),sep=""), yes = paste("Temp: P < 0.01")),
           x=6,y=50000)+
  geom_point(shape = 16, size = 1)+
  scale_color_manual(values = c("#006D2C","#08519C"),breaks = c("FW","SW"),labels = c("Freshwater","Seawater"))+
  
  
  labs(col = "Water",
       x = expression("Body mass (g)"), 
       y = expression("MO"["2,max"]*" (µmol h"^"-1"*")"));p5


## Jitter plot of residuals
p6<-
  ggplot(
    data = data.frame(
      resid = resid,
      sal = sal
    ),
    mapping = aes(x=sal,y=resid,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("residual log"[10]*"[MO"["2,max"]*" (µmol h"^"-1"*")]"))+
  ylim(c(
    min(data.frame(resid = resid,sal = sal)$resid),
    1.2*max(data.frame(resid = resid,sal = sal)$resid)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*resid), vjust=0,annotation=ifelse(test = paov$Pf<0.001, no = paste("P = ",signif(paov$Pf,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black") 
p6



pdf("./Figures/6B.pdf",width = 8/2.54,height = 8/2.54,useDingbats = F)
plot_grid(p6,nrow = 1,ncol =1,labels = "B",label_size = 12)
dev.off()

pdf("./Figures/6C.pdf",width = 8/2.54,height = 8/2.54,useDingbats = F)
plot_grid(p5,nrow = 1,ncol =1,labels = "C",label_size = 12)
dev.off()





# Model estimates for MO2,max
FWSW.MO2.effects<-
  aggregate(
    resid~sal,
    data.frame(resid = resid,sal = sal),
    mean)
FWSW.MO2.effects
temperature = 10
bodymass = 100

# absolute and mass-specific MO2,max without correcting for FW/SW
10^(int+log10(bodymass)*slo1+temperature*slo2)
10^(int+log10(bodymass)*slo1+temperature*slo2)/bodymass


# absolute and mass-specific MO2,max in FW
10^(int+log10(bodymass)*slo1+temperature*slo2+FWSW.MO2.effects[1,2])
10^(int+log10(bodymass)*slo1+temperature*slo2+FWSW.MO2.effects[1,2])/bodymass

# absolute and mass-specific MO2,max in SW
10^(int+log10(bodymass)*slo1+temperature*slo2+FWSW.MO2.effects[2,2])
10^(int+log10(bodymass)*slo1+temperature*slo2+FWSW.MO2.effects[2,2])/bodymass





###############
### PCRIT ###
###############

# Import data
df<-read_xlsx("./data.xlsx",sheet = "Pcrit")
df<-as.data.frame(df)

df<-subset(x = df,select = c("Species","Salinity","Pcrit (mmHg)","Temp C","Body mass (g)"))
colnames(df)<-c("sp","sal","pcrit","temp","bm") # abbreviated column names
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate genus and species by underscore
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate species and subspecies by underscore


# Import tree
tree<-read.tree(file = "./mcc.nexus")

# Correct recent changes in species names
df$sp[which(df$sp=="Ambloplites_rupestri")]<-"Ambloplites_rupestris"
df$sp[which(df$sp=="Aristichthys_nobilis")]<-"Hypophthalmichthys_nobilis"
df$sp[which(df$sp=="Carassius_auratus_grandoculis")]<-"Carassius_auratus"
df$sp[which(df$sp=="Ctenopharyngodon_piceus")]<-"Ctenopharyngodon_idella"
df$sp[which(df$sp=="Cyprinodon_ariegatus")]<-"Cyprinodon_variegatus_variegatus"
df$sp[which(df$sp=="Cyprinodon_variegatus")]<-"Cyprinodon_variegatus_variegatus"
df$sp[which(df$sp=="Apogon_compressus")]<-"Ostorhinchus_compressus"
df$sp[which(df$sp=="Apogon_cyanosoma")]<-"Ostorhinchus_cyanosoma"
df$sp[which(df$sp=="Apogon_exostigma")]<-"Pristiapogon_exostigma"
df$sp[which(df$sp=="Apogon_fragilis")]<-"Zoramia_fragilis"
df$sp[which(df$sp=="Apogon_leptacanthus")]<-"Zoramia_leptacantha"
df$sp[which(df$sp=="Asterropteryx_semipunctatus")]<-"Asterropteryx_semipunctata"
df$sp[which(df$sp=="Chaetopsylla_globiceps\r\n")]<-"Clinocottus_globiceps"
df$sp[which(df$sp=="Fundulus_heteroclitus")]<-"Fundulus_heteroclitus_heteroclitus"
df$sp[which(df$sp=="Ostorhinchus_doederleini")]<-"Apogon_doederleini"
df$sp[which(df$sp=="Paragobiodon_xanthosomus")]<-"Paragobiodon_xanthosoma"
df$sp[which(df$sp=="Ctenopharyngodon_idellus")]<-"Ctenopharyngodon_idella"
df$sp[which(df$sp=="Amblygobius_rainfordi")]<-"Koumansetta_rainfordi"
df$sp[which(df$sp=="Helcogramma_medium")]<-"Bellapiscis_medius"
df$sp[which(df$sp=="Lumpeninae_lumpenus")]<-"Lumpenus_lampretaeformis"
df$sp[which(df$sp=="Pagrus_auratus")]<-"Sparus_aurata"

# I chose a random species from these genera
df$sp[which(df$sp=="Pagothenia")]<-"Pagothenia_borchgrevinki" 
df$sp[which(df$sp=="Forsterygion_Sp.")]<-"Forsterygion_capito" 

# I have removed this species, as I cannot find it in the phylogeny
df <- df[which(df$sp!="Gobiodon_erythrospilus"),] 

# I have removed these species, as they are reported both as SW and FW in the data set
df <- df[which(df$sp!="Cottus_asper"),] 
df <- df[which(df$sp!="Cyprinodon_variegatus_variegatus"),] 
df <- df[which(df$sp!="Leptocottus_armatus"),] 
df <- df[which(df$sp!="Oncorhynchus_mykiss"),] 

# Has any species changed name?
df$sp[is.na(match(df$sp,tree$tip.label))]

# Step 1: Evaluate if pcrit depends on body mass
df.na<-na.omit(df)
tree<-keep.tip(tree,df.na$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)

prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
tree$node.label<-seq(1,length(tree$node.label),1)
inv.phylo<-MCMCglmm::inverseA(tree,nodes="TIPS",scale=TRUE)

glmm.bm<-
  MCMCglmm::MCMCglmm(
    pcrit~log10(bm),
    random=~sp,
    family="gaussian",
    ginverse=list(sp=inv.phylo$Ainv),
    prior=prior,
    data=df.na,
    nitt=1000000,
    burnin=50000,
    thin=1000)
summary(glmm.bm) 
# conclusion: pcrit does not depend on body mass. P = 0.918. Include studies that do not report body mass



# conclusion: thickness does not depend on body mass. P = 0.8163. Include studies that do not report body mass

p11.pcrit<-ggplot(data = df, mapping = aes(x = bm, y = pcrit, col = sal))+
  scale_x_continuous(trans = "log10",
                     labels = comma)+
  annotate(size = 6/3,geom = "text",
           label = ifelse(test = summary(glmm.bm)$solutions[2,5]<0.001, no = paste("P = ",signif(summary(glmm.bm)$solutions[2,5],3),sep=""), yes = paste("P < 0.001")),
           x=1,y=125,fsize = 8)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = "none"
  )+
  geom_abline(slope = summary(glmm.bm)$solutions[2,1],intercept = summary(glmm.bm)$solutions[1,1])+
  geom_point(shape = 16, size = 1)+
  scale_color_manual(values = c("#006D2C","#08519C"),breaks = c("FW","SW"),labels = c("Freshwater","Seawater"))+
  labs(col = "Water",
       x = expression("Body mass (g)"), 
       y = expression("P"["crit"]*" (mmHg)"));p11.pcrit



pdf("./Figures/4C.pdf",width = 8/2.54,height = 8/2.54,useDingbats = F)
plot_grid(p11.pcrit,nrow = 1,ncol =1,labels = "C",label_size = 12)
dev.off()








# Step 2: Evaluate if pcrit depends on temperature
tree<-read.tree(file = "./mcc.nexus")
tree<-keep.tip(tree,df$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)

prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
tree$node.label<-seq(1,length(tree$node.label),1)
inv.phylo<-MCMCglmm::inverseA(tree,nodes="TIPS",scale=TRUE)
glmm.temp<-
  MCMCglmm::MCMCglmm(
    pcrit~temp,
    random=~sp,
    family="gaussian",
    ginverse=list(sp=inv.phylo$Ainv),
    prior=prior,
    data=df,
    nitt=1000000,
    burnin=50000,
    thin=1000)
summary(glmm.temp) 
# conclusion: no effect of temperature on pcrit (P = 0.102). Analyze all pcrit without taking temperature into account. 


# Step 3: Evaluate if pcrit depend on salinity

# Genereate named vector for the residuals
pcrit<-aggregate(pcrit~sp,df,median)
pcrit<-setNames(pcrit$pcrit,pcrit$sp)
pcrit

# Genereate named vector for salinity
sal<-aggregate(sal~sp,df,unique)
sal<-setNames(sal$sal,sal$sp)
sal





#phyloaov
paov<-phylANOVA(tree = tree,sal,pcrit,nsim = 50000)
paov
phylosig(tree = tree,x = resid,method = "lambda",test = T)


# Set colors
col = to.matrix(sal[tree$tip.label],seq=sort(unique(sal)))[,1]

col[which(col == 0)]<-"#08519C"
col[which(col == 1)]<-"#006D2C"
col<-col[tree$tip.label]


## Jitter plot of Pcrit

p7<-
  ggplot(
    data = data.frame(
      pcrit = pcrit,
      sal = sal
    ),
    mapping = aes(x=sal,y=pcrit,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("P"[crit]*" (mmHg)"))+
  ylim(c(
    min(data.frame(pcrit = pcrit,sal = sal)$pcrit),
    1.2*max(data.frame(pcrit = pcrit,sal = sal)$pcrit)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*pcrit), vjust=0,annotation=ifelse(test = paov$Pf<0.001, no = paste("P = ",signif(paov$Pf,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black") 
p7

pdf("./Figures/4B.pdf",width = 8/2.54,height = 8/2.54,useDingbats = F)
plot_grid(p7,nrow = 1,ncol =1,labels = "B",label_size = 12)
dev.off()



##########
### Gd ###
##########

# Import data
df<-read_xlsx("./data.xlsx",sheet = "Thickness")
df<-as.data.frame(df)
df
df<-subset(x = df,select = c("Species","Salinity","Body mass (g) for GSA","Gd (umol mmHg-1   min-1  kg-1)"))
colnames(df)<-c("sp","sal","bm","gd") # abbreviated column names
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate genus and species by underscore
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate species and subspecies by underscore
df
df<-df[!is.na(df$bm),] # remove studies that do not report body mass
df

df$gdg<-df$gd

# Calculate absolute Gd
df$gd<-df$gd*df$bm

df<-na.omit(df)

# Import tree
tree<-read.tree(file = "./mcc.nexus")

# Correct recent changes in species names
df$sp[which(df$sp=="Salmo_gairdneri")]<-"Oncorhynchus_mykiss"
df$sp[which(df$sp=="Ictalurus_nebulosus")]<-"Ameiurus_nebulosus"
df$sp[which(df$sp=="Scornber_scombrus")]<-"Scomber_scombrus"
df$sp[which(df$sp=="Katsuuionis_pelamis")]<-"Katsuwonus_pelamis"
df$sp[which(df$sp=="Notropis_cornutus")]<-"Luxilus_cornutus"
df$sp[which(df$sp=="Gadus_virens")]<-"Pollachius_virens"
df$sp[which(df$sp=="Acanthocottus_scorpius")]<-"Myoxocephalus_scorpius"
df$sp[which(df$sp=="Anguilla_vulgaris")]<-"Anguilla_anguilla"
df$sp[which(df$sp=="Amerurus_nebulosus")]<-"Ameiurus_nebulosus"
df$sp[which(df$sp=="Platichtys_flesus")]<-"Platichthys_flesus"

# Remove european perch
df <- df[which(df$sp!="Perca_fluviatilis"),]

# Has any species changed name?
df$sp[is.na(match(df$sp,tree$tip.label))]


# Mass specific Gd without phylogenetic correction
df.0.gd<-
  cbind(
    aggregate(gdg~sp,df,mean),
    aggregate(sal~sp,df,unique)[,2]
  )


colnames(df.0.gd)[3]<-"sal"

p.value.gdg<-t.test(gdg~sal,df.0.gd)$p.value
p.value.gdg


p0.gdg<-
  ggplot(
    data = df.0.gd,
    mapping = aes(x=sal,y=gdg,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("Mass-specific G"[d]*" (µmol mmHg"^"-1"*" h"^"-1"*" g"^"-1"*")"))+
  ylim(c(
    min(df.0.gd$gdg),
    1.2*max(df.0.gd$gdg)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*df.0.gd$gdg), vjust=0,annotation=ifelse(test = p.value.gdg<0.001, no = paste("P = ",signif(p.value.gdg,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black");p0.gdg









# Prune tree to only represent species in the data set
tree<-keep.tip(tree,df$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)

# Set up glmm model
prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
tree$node.label<-seq(1,length(tree$node.label),1)
inv.phylo<-MCMCglmm::inverseA(tree,nodes="TIPS",scale=TRUE)


glmm1.1<-
  MCMCglmm::MCMCglmm(
    log10(gd)~log10(bm),
    random=~sp,
    family="gaussian",
    ginverse=list(sp=inv.phylo$Ainv),
    prior=prior,
    data=df,
    nitt=1000000,
    burnin=500000,
    thin=10000)
summary(glmm1.1) # There is a significant positive effect of body mass on Gd



# extract intercept and slope
summary(glmm1.1)$solutions
int<-summary(glmm1.1)$solutions[1,1];int
slo<-summary(glmm1.1)$solutions[2,1];slo


# Calculate residuals
df$resid<-log10(df$gd)-(log10(df$bm)*slo+int)



# Genereate named vector for the residuals
resid<-aggregate(resid~sp,df,median)
resid<-setNames(resid$resid,resid$sp)
resid.gd<-resid

# Genereate named vector for salinity
sal<-aggregate(sal~sp,df,unique)
sal<-setNames(sal$sal,sal$sp)


#phyloaov
paov<-phylANOVA(tree = tree,sal,resid,nsim = 50000)
paov
phylosig(tree = tree,x = resid,method = "lambda",test = T)


# Set colors
col = to.matrix(sal[tree$tip.label],seq=sort(unique(sal)))[,1]
col[which(col == 0)]<-"#08519C"
col[which(col == 1)]<-"#006D2C"
col<-col[tree$tip.label]

# Set up axis text format
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# Plot bm and Gd scatter plot
p8<-ggplot(data = df, mapping = aes(x = bm, y = gd, col = sal))+
  scale_x_continuous(trans = "log10",labels = comma)+
  scale_y_continuous(trans = "log10",labels = comma)+
  annotate(size = 6/3,geom = "text",
           label = ifelse(test = summary(glmm1.1)$solutions[2,5]<0.001, no = paste("P = ",signif(summary(glmm1.1)$solutions[2,5],3),sep=""), yes = paste("P < 0.001")),
           x=6,y=10000)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "none"
    )+
  geom_point(shape = 16, size = 1)+
  scale_color_manual(values = c("#006D2C","#08519C"),breaks = c("FW","SW"),labels = c("Freshwater","Seawater"))+
  geom_abline(slope = slo,intercept = int)+
  
  labs(col = "Water",
       x = expression("Body mass (g)"), 
       y = expression("G"[d]*" (µmol mmHg"^"-1"*" h"^"-1"*")"));p8



## Jitter plot of residuals
p9<-
  ggplot(
    data = data.frame(
      resid = resid,
      sal = sal
    ),
    mapping = aes(x=sal,y=resid,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        )+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("residual log"[10]*"[G"[d]*" (µmol mmHg"^"-1"*" h"^"-1"*")]"))+
  ylim(c(
    min(data.frame(resid = resid,sal = sal)$resid),
    1.2*max(data.frame(resid = resid,sal = sal)$resid)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*resid), vjust=0,annotation=ifelse(test = paov$Pf<0.001, no = paste("P = ",signif(paov$Pf,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black") 
p9




# Model estimates for Gd
FWSW.Gd.effects<-
  aggregate(
    resid~sal,
    data.frame(resid = resid,sal = sal),
    mean)
FWSW.Gd.effects

bodymass = 100

# absolute and mass-specific Gd without correcting for FW/SW
10^(int+log10(bodymass)*slo)
10^(int+log10(bodymass)*slo)/bodymass


# absolute and mass-specific Gd in FW
10^(int+log10(bodymass)*slo+FWSW.Gd.effects[1,2])
10^(int+log10(bodymass)*slo+FWSW.Gd.effects[1,2])/bodymass

# absolute and mass-specific Gd in SW
10^(int+log10(bodymass)*slo+FWSW.Gd.effects[2,2])
10^(int+log10(bodymass)*slo+FWSW.Gd.effects[2,2])/bodymass








#################
### Thickness ###
#################

# Import data
df<-read_xlsx("./data.xlsx",sheet = "Thickness")
df<-as.data.frame(df)
df
df<-subset(x = df,select = c("Species","Salinity","Water-blood thickness (um)","Body mass (g) for GSA"))
colnames(df)<-c("sp","sal","thick","bm") # abbreviated column names
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate genus and species by underscore
df$sp             <-sub("\\ ", "_", df$sp)                      # Separate species and subspecies by underscore

# Import tree
tree<-read.tree(file = "./mcc.nexus")

# Correct recent changes in species names
df$sp[which(df$sp=="Salmo_gairdneri")]<-"Oncorhynchus_mykiss"
df$sp[which(df$sp=="Ictalurus_nebulosus")]<-"Ameiurus_nebulosus"
df$sp[which(df$sp=="Scornber_scombrus")]<-"Scomber_scombrus"
df$sp[which(df$sp=="Channa_punctatus")]<-"Channa_punctata"
df$sp[which(df$sp=="Clarias_mossambicus")]<-"Clarias_gariepinus"
df$sp[which(df$sp=="Euthynnus_afinis")]<-"Euthynnus_affinis"
df$sp[which(df$sp=="Microstornus_kitt")]<-"Microstomus_kitt"
df$sp[which(df$sp=="Solea_variegata")]<-"Microchirus_variegatus"
df$sp[which(df$sp=="Tetrapturus_audax")]<-"Kajikia_audax"
df$sp[which(df$sp=="Katsuuionis_pelamis")]<-"Katsuwonus_pelamis"
df$sp[which(df$sp=="Notropis_cornutus")]<-"Luxilus_cornutus"
df$sp[which(df$sp=="Gadus_virens")]<-"Pollachius_virens"
df$sp[which(df$sp=="Acanthocottus_scorpius")]<-"Myoxocephalus_scorpius"
df$sp[which(df$sp=="Anguilla_vulgaris")]<-"Anguilla_anguilla"
df$sp[which(df$sp=="Amerurus_nebulosus")]<-"Ameiurus_nebulosus"
df$sp[which(df$sp=="Platichtys_flesus")]<-"Platichthys_flesus"

# Remove Chaenocephalus_aceratus
df <- df[which(df$sp!="Chaenocephalus_aceratus"),]

# Has any species changed name?
df$sp[is.na(match(df$sp,tree$tip.label))]


# Thickness without phylogenetic correction
df.0.thick<-
  cbind(
    aggregate(thick~sp,df,mean),
    aggregate(sal~sp,df,unique)[,2]
  )


colnames(df.0.thick)[3]<-"sal"

p.value.thick<-t.test(thick~sal,df.0.thick)$p.value
p.value.thick


p0.thick<-
  ggplot(
    data = df.0.thick,
    mapping = aes(x=sal,y=thick,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("Thickness (µm)"))+
  ylim(c(
    min(df.0.thick$thick),
    1.2*max(df.0.thick$thick)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*df.0.thick$thick), vjust=0,annotation=ifelse(test = p.value.thick<0.001, no = paste("P = ",signif(p.value.thick,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black");p0.thick





# Step 1: Evaluate if thickness depends on body mass
df.na<-na.omit(df)
tree<-keep.tip(tree,df.na$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)

prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
tree$node.label<-seq(1,length(tree$node.label),1)
inv.phylo<-MCMCglmm::inverseA(tree,nodes="TIPS",scale=TRUE)

glmm.thick.bm<-
  MCMCglmm::MCMCglmm(
    thick~log10(bm),
    random=~sp,
    family="gaussian",
    ginverse=list(sp=inv.phylo$Ainv),
    prior=prior,
    data=df.na,
    nitt=1000000,
    burnin=50000,
    thin=1000)
summary(glmm.thick.bm) 

# conclusion: thickness does not depend on body mass. P = 0.8163. Include studies that do not report body mass

p11<-ggplot(data = df, mapping = aes(x = bm, y = thick, col = sal))+
  scale_x_continuous(trans = "log10",
                     labels = comma)+
  annotate(size = 6/3,geom = "text",
           label = ifelse(test = summary(glmm.thick.bm)$solutions[2,5]<0.001, no = paste("P = ",signif(summary(glmm.thick.bm)$solutions[2,5],3),sep=""), yes = paste("P < 0.001")),
           x=7,y=10,fsize = 8)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = "none"
    )+
  geom_abline(slope = summary(glmm.thick.bm)$solutions[2,1],intercept = summary(glmm.thick.bm)$solutions[1,1])+
  geom_point(shape = 16, size = 1)+
  scale_color_manual(values = c("#006D2C","#08519C"),breaks = c("FW","SW"),labels = c("Freshwater","Seawater"))+
  labs(col = "Water",
       x = expression("Body mass (g)"), 
       y = expression("Thickness (µm)"));p11








# Prune tree to only represent species in the data set
tree<-keep.tip(tree,df$sp)
tree<-ladderize(tree)
tree<-force.ultrametric(tree)

# Genereate named vector for the residuals
thick<-aggregate(thick~sp,df,median)
thick<-setNames(thick$thick,thick$sp)
thick

# Genereate named vector for salinity
sal<-aggregate(sal~sp,df,unique)
sal<-setNames(sal$sal,sal$sp)
sal


#phyloaov
paov<-phylANOVA(tree = tree,sal,thick,nsim = 50000)
paov
phylosig(tree = tree,x = thick,method = "lambda",test = T)

# Set colors
col = to.matrix(sal[tree$tip.label],seq=sort(unique(sal)))[,1]
col[which(col == 0)]<-"#08519C"
col[which(col == 1)]<-"#006D2C"
col<-col[tree$tip.label]




## Jitter plot of residuals
p10<-
  ggplot(
    data = data.frame(
      thick = thick,
      sal = sal
    ),
    mapping = aes(x=sal,y=thick,color=sal)
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_jitter(shape =16, width = 0.3)+
  scale_color_manual(values = c("#006D2C","#08519C"))+
  scale_x_discrete(labels = c("Freshwater","Seawater"))+
  labs(x = expression("Water"),
       y = expression("Thickness (µm)"))+
  ylim(c(
    min(data.frame(thick = thick,sal = sal)$thick),
    1.2*max(data.frame(thick = thick,sal = sal)$thick)))+
  geom_signif(comparisons=list(c("FW", "SW")),y_position = max(1.1*thick), vjust=0,annotation=ifelse(test = paov$Pf<0.001, no = paste("P = ",signif(paov$Pf,3),sep=""), yes = paste("P < 0.001")),size = 0.2,textsize = 6/3,color="black") 
p10






##########################
### Multipanel figures ### 
##########################



pdf("./Figures/Fig. 1.pdf",width = 18/2.54,height = 24/2.54,useDingbats = F)
plot_grid(p0,p1,p2,p0.gdg,p8,p9,p0.thick,p11,p10,p0.p50,p3,p4,nrow = 4,ncol = 3,align = "hv",labels = c("A","E","I","B","F","J","C","G","K","D","H","L"),label_size = 12,hjust = -3)
dev.off()


