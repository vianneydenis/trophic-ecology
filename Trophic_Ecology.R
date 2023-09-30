knitr::purl("Trophic_Ecology.Rmd", documentation = F)

library (ggplot2) # data visualization
library(GGally) # data visualization
library (reshape2) # data manipulation
library(plyr) # data manipulation
library(tidyr) # data manipulation
library(dplyr) # data manipulation
library(splancs) # display and analysis of spatial point pattern data
library(R2WinBUGS) # Bayesian analysis
library(SIBER) # trophic niche  
library(MixSIAR) # mixing model 

# Load your data
Dataset <- read.csv("Data/Dataset.csv") # Import a file

#View the first 10 rows of data
head(Dataset) # see the head

# Make a simplified dataset 
Data <- Dataset # Make a copy of the object
Data$CN <- (Data$mgC/Data$Weight*100)/(Data$mgN/Data$Weight*100) # Compute CN ratio
Data[,c("Reference", "Date_Station_Type", "Date", "Station", "Other", "RunNo" )] <-NULL # add NULL in the useless variables
head(Data) 

# Some quick summary
## all categories
Data.sum.1<-ddply(Data, c("Season", "Category"), summarise,
                count = length(Category),
                mC = round(mean(d13C, na.rm=T), digits=1), sdC = round(sd(d13C, na.rm=T), digits=1), 
                mN = round(mean(d15N, na.rm=T), digits=1), sdN = round(sd(d15N, na.rm=T), digits=1))
head(Data.sum.1)
## coral only
Coral<-subset(Data, Type == "coral")
Coral.sum.1<-ddply(Coral, c("Season", "Category"), summarise,
                count = length(Category),
                mC = round(mean(d13C), digits=1), sdC = round(sd(d13C), digits=1), 
                mN = round(mean(d15N), digits=1), sdN = round(sd(d15N), digits=1))
Coral.sum.1

# A subset of the data set
A_subpinnata_spring<-subset(Data, Category == "A_subpinnata" | Season == "spring")
head(A_subpinnata_spring)

# Plotting isotope data: d13C only (or any other variable)

New.data<-subset(Data, Category == "A_subpinnata" | Category == "E_cavolini")

Iso.boxplot<-ggplot(New.data, aes(x=Category, group = Category, y=d13C, colour=Category)) +                 
  geom_boxplot() +
  geom_point(position="jitter")+
  facet_grid(. ~ Season)

Iso.boxplot

ggsave("Plots/d13C.pdf")

# Plotting isotope data: All at once with a step of melting and faceting the data set 

Melt.data<-melt(New.data, measure.vars  = c("d13C", "d15N", "CN"), variable.name= "variable", value.name="value") # idvar to explain

Melt.boxplot<-ggplot(Melt.data, aes(x=Category, group = Category, y=value, colour=Category, fill=Category)) +
  geom_boxplot(alpha=0.3, outlier.shape = NA) +
  geom_point(position="jitter") +
  facet_grid(variable ~ Season, scales = "free_y") +
  theme_bw()

Melt.boxplot

# Carbonate effect

New.data$perc_C<-New.data$mgC/(New.data$mgC+New.data$mgN)

Carbonate.plot<-ggplot(New.data, aes(x=perc_C, y=d13C)) +
  geom_point() +
  geom_smooth(method = "lm", se=T,formula=y~x) +
  facet_grid(Category ~ Season, scales = "free_x")

Carbonate.plot 

# Lipid effect raw
Lipid.plot<-ggplot(New.data, aes(x=CN, y=d13C)) +
  geom_point() +
  geom_smooth(method = "lm", se=T,formula=y~x) +
  facet_grid(Category ~ Season)

Lipid.plot 

# Lipid effect threshold
Lipid.boxplot<-ggplot(subset(Data, Type == "coral"), aes(x=Category, group = Category, y=CN, colour=Category)) +
  geom_boxplot() +
  geom_point(position="jitter")+
  ylim(3,9) +
  geom_hline(aes(yintercept=7), colour="red") +
  facet_grid(. ~ Season) +
  theme_bw()

Lipid.boxplot  

# Pair plot
ggpairs(New.data[,c("d15N","d13C", "mgC", "mgN", "CN")])

# Raw biplot
Iso.biplot<-ggplot(Data, aes(x = d13C, y = d15N, colour = Category, shape = Type)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Season) +
  theme_bw() +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))

Iso.biplot

ggsave("Plots/Iso_biplots.pdf")

# Summary biplot
Data.sum<-ddply(Data, c("Season", "Category"), summarise,
                d13Cmn=mean(d13C, na.rm=T), # mean
                d13Csd=sd(d13C, na.rm=T),#standard deviation
                d13Cse=sd(d13C, na.rm=T)/sqrt(length(Data$d13C[!is.na(Data$d13C)])), #standard error
                d15Nmn=mean(d15N, na.rm=T),
                d15Nsd=sd(d15N, na.rm=T),
                d15Nse=sd(d15N, na.rm=T)/sqrt(length(Data$d15N[!is.na(Data$d15N)])))

Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)

Sum.biplot<-ggplot(Data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Category, shape =Category)) + 
  geom_point(size=3) + 
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
  facet_grid(. ~ Season)+
  theme_bw()

Sum.biplot

ggsave("Plots/Sum_biplot.pdf")

# Coral biplot
Coral.biplot<-ggplot(subset(Data, Category == "A_subpinnata" | Category == "E_cavolini"), aes(x=d13C, y=d15N, colour = Category)) +
  geom_point() +
  stat_ellipse() + # 95%
  facet_grid(. ~ Season)+
  theme_bw()

Coral.biplot

ggsave("Plots/Coral_biplot.pdf")


knitr::include_graphics("Figures/2_sources.png")

# Endmember selection
endmembers.sum<-Data.sum.1
PNP<-subset(endmembers.sum, Category == "pico-nanoplankton")
POM<-subset(endmembers.sum, Category == "sediment")
Season<-c("autumn", "spring")
d13Cpel<-c(-23.4, -23.8)
d15Npel<-c(3.1, 5.7)
d13Clit<-c(-1.6, 0.2)
d15Nlit<-c(3.4, 2.1)
endmembers<-data.frame(Season, d13Cpel, d15Npel, d13Clit, d15Nlit)
mm.data<-merge(Data, endmembers, by = "Season")
head(mm.data)

# Resource boxplot raw
mm.data$PEL<-(mm.data$d13C - mm.data$d13Clit) / (mm.data$d13Cpel - mm.data$d13Clit)
mm.data$PEL<-round(mm.data$PEL, digits=2)
head(mm.data)

resource.boxplot.1<-ggplot(subset(mm.data, Category == "A_subpinnata" | Category == "E_cavolini"), aes(x=Category, group = Category, y=PEL, colour=Category)) +
                  geom_boxplot() +
                  facet_grid(. ~ Season) +
                  theme_bw()
resource.boxplot.1

# Resource boxplot corrected
mm.data$PELcorr<-mm.data$PEL
mm.data$PELcorr[mm.data$PELcorr < 0]<-0
mm.data$PELcorr[mm.data$PELcorr > 1]<-1
head(mm.data)

resource.boxplot.2<-ggplot(subset(mm.data, Category == "A_subpinnata" | Category == "E_cavolini"), aes(x=Category, group = Category, y=PELcorr, colour=Category)) +
                  geom_boxplot() +
                  facet_grid(. ~ Season) +
                  theme_bw()
resource.boxplot.2


knitr::include_graphics("Figures/3_sources.png")
knitr::include_graphics("Figures/Mixing_model.png")


### consumer data
Cons <- New.data # consumer data set
Cons[,c("Type", "Weight", "mgN", "mgC", "CN", "perc_C")]<-NULL
write.csv(Cons, "Data/Cons.csv", row.names=T) 
# Note for Posit path "/cloud/project/Cons.csv" to ignore

Sub.data<-subset(Data, Category == "mesoplankton" | Category == "microplankton" | Category == "pico-nanoplankton" | Category == "sediment" )
Data.sum.2<-ddply(Sub.data, c("Season", "Category"), summarise,
                  Meand13C=mean(d13C, na.rm=T), #mean
                  SDd13C=sd(d13C, na.rm=T),#standard deviation
                  Meand15N=mean(d15N, na.rm=T),
                  SDd15N=sd(d15N, na.rm=T))

Sources<-Data.sum.2[,c(2,1,3,4,5,6)] # reshaping table
Sources$n<-rep(1000,8) # treating n as unknown (see MixSiAR manual)
write.csv(Sources, "Data/Sources.csv", row.names=F) 
# Note for Posit path "/cloud/project/Sources.csv" to ignore

Meand13C<-c(1.0, 1.0, 1.0,1.0)
SDd13C<-c(0.4, 0.4, 0.4, 0.4)
Meand15N<-c(2.5, 2.5, 2.5, 2.5)
SDd15N<-c(1, 1, 1, 1)
Tef<- data.frame(Meand13C, SDd13C, Meand15N, SDd15N)
rownames(Tef)<-c("mesoplankton" , "microplankton" , "pico-nanoplankton" , "sediment" )
write.csv(Tef, "Data/Tef.csv", row.names=T)
# Note for Posit path "/cloud/project/Tef.csv" to ignore


# Consumers

mix <- load_mix_data(filename = "Data/Cons.csv", #"/cloud/project/Cons.csv"
                     iso_names = c("d13C","d15N"),
                     factors   = c("Season", "Category"),
                     fac_random = c(FALSE,FALSE), # very important here  T / F
                     fac_nested = c(TRUE,FALSE), # very important here F / T
                     cont_effects = NULL) # if you have gradient

# Sources

source <- load_source_data(filename = "Data/Sources.csv", #"/cloud/project/Sources.csv"
                           source_factors = "Season", 
                           conc_dep = FALSE, # concentration dependent 
                           data_type = "means", 
                           mix)

# TEFs

discr <- load_discr_data(filename = "Data/Tef.csv", mix) 

# Make an isospace plot
plot_data(filename="Plots/isospace_plot", plot_save_pdf=FALSE, plot_save_png=TRUE, mix, source, discr)

# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
plot_prior(filename="Plots/prior_plot", alpha.prior=c(1),source, plot_save_png = TRUE, plot_save_pdf=FALSE)

# View an 'informative' prior 
plot_prior(alpha.prior=c(0.1,0.5,1,1),source, plot_save_png = FALSE, plot_save_pdf=FALSE)

# Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

run <- list(chainLength=1000, burn=500, thin=1, chains=3, calcDIC=TRUE)

## # not run, youcan weight your priors in the lines below
## jags.1 <- run_model(run="test", mix, source, discr, model_filename, alpha.prior = 1, resid_err, process_err)
## 
## # jags.1 <- run_model(run="normal", mix, source, discr, model_filename, alpha.prior = 1, resid_err, process_err)
## 

## #not run
## output_options <- list(summary_save = TRUE,
##                        summary_name = "summary_statistics",
##                        sup_post = FALSE,
##                        plot_post_save_pdf = FALSE,
##                        plot_post_name = "posterior_density",
##                        sup_pairs = FALSE,
##                        plot_pairs_save_pdf = FALSE,
##                        plot_pairs_name = "pairs_plot",
##                        sup_xy = FALSE, #
##                        plot_xy_save_pdf = FALSE,
##                        plot_xy_name = "xy_plot",
##                        gelman = TRUE,
##                        heidel = FALSE,
##                        geweke = FALSE,
##                        diag_save = TRUE,
##                        diag_name = "diagnostics",
##                        indiv_effect = FALSE,
##                        plot_post_save_png = TRUE,
##                        plot_pairs_save_png = FALSE,
##                        plot_xy_save_png = FALSE)

## # not run, figures from model output "normal"
## output_JAGS(jags.1, mix, source, output_options)

readLines("diagnostics.txt")
knitr::include_graphics("Figures/posterior_density_diet_p_spring_A_subpinnata.png")
knitr::include_graphics("Figures/posterior_density_diet_p_autumn_A_subpinnata.png")
knitr::include_graphics("Figures/posterior_density_diet_p_spring_E_cavolini.png")
knitr::include_graphics("Figures/posterior_density_diet_p_autumn_E_cavolini.png")


#Siber biplots
Siber.biplots<-ggplot(Data, aes(x = d13C, y = d15N, colour = Category)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Season) +
  theme_bw() +
  ylab("iso2") +
  xlab("iso1")

Siber.biplots

# Subset coral
Siber.group<-subset(Dataset, Category == "A_subpinnata" | Category == "E_cavolini")[,c("d13C", "d15N", "Category", "Season")]
head(Siber.group)

# Siber group biplots
Siber.group.biplots<-ggplot(Siber.group, aes(x = d13C, y = d15N, colour = Category)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Season) +
  theme_bw() +
  ylab("iso2") +
  xlab("iso1")

Siber.group.biplots

# Add ellipses

Siber.group.biplots+
  stat_ellipse(position="identity", level=0.4, linewidth=2)+
  stat_ellipse(position="identity", level=0.95, linewidth=1)

# Variable name requirment SIBER + create SIBER object
colnames(Siber.group) <- c('iso1','iso2','group', 'community') 
Siber.g.example <- createSiberObject(Siber.group)


# Create lists of plotting arguments to be passed onwards to each of the three plotting functions.
# Plot SIBER data

community.hulls.args <- list(col = 1, lty = 0, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

par(mfrow=c(1,1))

plotSiberObject(Siber.g.example,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = T, group.ellipses.args,
                  group.hulls = T, group.hull.args,
                  bty = "L",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~'\u2030'),
                  ylab = expression({delta}^15*N~'\u2030')
                  )

# Calculate summary statistics for each group: TA, SEA and SEAc
Group.ML <- groupMetricsML(Siber.g.example)
print(Group.ML)

# The overlap of the maximum likelihood fitted standard ellipses are 
# estimated using
A_subpinnata.overlap <- maxLikOverlap("autumn.A_subpinnata", "spring.A_subpinnata", Siber.g.example, 
                              p.interval = NULL, n = 100)
A_subpinnata.overlap

# the overlap between the corresponding 95% prediction ellipses is given by

A_subpinnata.95.overlap <- maxLikOverlap("autumn.A_subpinnata", "spring.A_subpinnata", Siber.g.example, 
                                      p.interval = 0.95, n = 100)
A_subpinnata.95.overlap

# so in this case, the overlap as a proportion of the non-overlapping area of 
# the two ellipses, would be

A_subpinnata.over <- A_subpinnata.overlap[3] / (A_subpinnata.overlap[2] + 
                                          A_subpinnata.overlap[1] -
                                         A_subpinnata.overlap[3])
A_subpinnata.over

# repeat for other species

# The overlap of the maximum likelihood fitted standard ellipses are 
# estimated using
E_cavolini.overlap <- maxLikOverlap("autumn.E_cavolini", "spring.E_cavolini", Siber.g.example, 
                                      p.interval = NULL, n = 100)
E_cavolini.overlap

E_cavolini.95.overlap <- maxLikOverlap("autumn.E_cavolini", "spring.E_cavolini", Siber.g.example, 
                                      p.interval = 0.95, n = 100)
E_cavolini.95.overlap

E_cavolini.over <- E_cavolini.overlap[3] / (E_cavolini.overlap[2] + 
                                          E_cavolini.overlap[1] -
                                         E_cavolini.overlap[3])
E_cavolini.over


# default model
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# default priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# run the model
ellipses.posterior <- siberMVN(Siber.g.example, parms, priors) # MVN: Multivariate Normal distribution


SEA.B <- siberEllipses(ellipses.posterior)

siberDensityPlot(SEA.B,
                xticklabels = c('Aut sub', 'Spr sub', 'Aut cav', 'Spr cav'), 
                xlab = c("Community | Group"),
                ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                bty = "L",
                las = 1,
                main = "SIBER ellipses on each group"
                )

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), Group.ML[3,], col="red", pch = "x", lwd = 2)



# Calculate some credible intervals 
cr.p <- c(0.95, 0.99) # vector of quantiles

# do similar to get the modes, taking care to pick up multimodal posterior
# distributions if present
SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T)
SEA.B.modes

# call to hdrcde:hdr using lapply()
SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p)
SEA.B.credibles

# Subset coral
Siber.comm<-Data[,c("d13C", "d15N", "Category", "Season")]
colnames(Siber.comm) <- c('iso1','iso2','group', 'community') 

# A bit of cooking to remove the NA values
Siber.comm.corr<-Siber.comm %>% 
  group_by(group, community) %>% 
  mutate_at(vars(iso1, iso2), ~replace_na(., mean(., na.rm = TRUE)))

Siber.comm.corr<-as.data.frame(Siber.comm.corr)

# Creating SIBER object
Siber.c.example <- createSiberObject(Siber.comm.corr)
Siber.c.example$sample.sizes

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)

par(mfrow=c(1,1))
plotSiberObject(Siber.c.example,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5,
                )

community.ML <- communityMetricsML(Siber.c.example) 
print(community.ML)

parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

ellipses.posterior <- siberMVN(Siber.c.example, parms, priors) #run the model

# extract the posterior means
mu.post <- extractPosteriorMeans(Siber.c.example, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)


# --------------------------------------
# Visualise the first community
# --------------------------------------
siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), 
                bty="L", ylim = c(0,100))

# add the ML estimates (if you want). Extract the correct means 
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(Siber.c.example$ML.mu[[1]][1,1,],
                                 Siber.c.example$ML.mu[[1]][1,2,]
                                 )
points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)

# --------------------------------------
# Visualise the second community
# --------------------------------------
siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), 
                bty="L", ylim = c(0,100))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(Siber.c.example$ML.mu[[2]][1,1,],
                                 Siber.c.example$ML.mu[[2]][1,2,]
)
points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)



par(mfrow=c(1,1))

siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"]),
                xticklabels = c("autumn", "spring"), 
                bty="L", 
                ylim = c(0,100),
                las = 1,
                ylab = "Total Area",
                xlab = "")

siberDensityPlot(cbind(layman.B[[1]][,"dY_range"], layman.B[[2]][,"dY_range"]),
                xticklabels = c("autumn", "spring"), 
                bty="L", ylim = c(0,10),
                las = 1,
                ylab = "Food chain length",
                xlab = "")
