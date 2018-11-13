library(readr) 
library(dplyr)
library(ggplot2)


"""Vectors created for the concentrations of each standard, used as the X axis"""
mixed_concentrations <- c(0.00,1.00,10.00,20.00,50.00,80.00,120.00,166.67)
glucose_concentrations <- c(0.0,10.0,50.0,100.0,150.0,200.0,250.0,277.0)

"""vector created for time"""
Hours <- c(0, 24, 48, 72)

"""All of the reads obtained from the recent run of standards in the LCRID (DAD for acetone)"""

"""MODIFY THIS PART OF THE CODE TO READ THE EXCEL FILE AS CSV, TAKE IN DATA DIRECTLY"""

lactate_standard <- c(6390.034,
                      31156.781,
                      201709.188,
                      394828.5,
                      945849.188,
                      1512594.125,
                      2324497.5,
                      3224856.0)

acetate_standard <- c(91.457,
                      6704.747,
                      59335.41,
                      115473.117,
                      273990.781,
                      438346.219,
                      672933.875,
                      924702.438)

acetone_standard <- c(315.645,
                      763.447,
                      239.091,
                      446.569,
                      1008.255,
                      1564.469,
                      2418.522,
                      3119.169)

butyrate_standard <- c(13.306,
                       28134.145,
                       223433.984,
                       442430.813,
                       1065843.0,
                       1715669.125,
                       2636440.75,
                       3659604.25)

butanol_standard <- c(11.826,
                      2900.865,
                      14247.027,
                      25887.775,
                      59313.008,
                      93563.586,
                      144127.266,
                      196249.797)

glucose_standard <- c(7.248,
                      12.198,
                      12.362,
                      10.208,
                      66.591,
                      13.67,
                      25.648,
                      223.358)

ethanol_standard <- c(91.457,
                      6704.747,
                      59335.41,
                      115473.117,
                      273990.781,
                      438346.219,
                      672933.875,
                      924702.438)

"""Fitting the linear model to an XY plot of concentration(x) to read (y)"""

lactate_gradient = lm(lactate_standard ~ mixed_concentrations)
acetate_gradient = lm(acetate_standard ~ mixed_concentrations)
acetone_gradient = lm(acetone_standard ~ mixed_concentrations)
butyrate_gradient = lm(butyrate_standard ~ mixed_concentrations)
butanol_gradient = lm(butanol_standard ~ mixed_concentrations)
ethanol_gradient = lm(ethanol_standard ~ mixed_concentrations)
glucose_gradient = lm(glucose_standard ~ glucose_concentrations)


"""insert vectors for all of the data reads"""


"""WT"""

wt_1_glucose <- c(4372473.5,
                  434320.219,
                  7388.459,
                  6948.68)

wt_2_glucose <- c(5757423,
                  802741.875,
                  3064.407,
                  3409.698)

wt_1_lactate <- c(7587.021,
                  27302.814,
                  26561.863,
                  24662.295)

wt_2_lactate <- c(9868.146,
                  24040.676,
                  24417.855,
                  26286.42)

wt_1_acetate <- c(1658.033,
                  314883.875,
                  293269.719,
                  273691.469)

wt_2_acetate <- c(2260.774,
                  218704.922,
                  274320.969,
                  286698.375)

wt_1_ethanol <- c(2784.144,
                  19375.146,
                  17580.416,
                  17479.922)

wt_2_ethanol <- c(3250.74,
                  13705.918,
                  16524.682,
                  16843.66)

wt_1_acetone <- c(2000.5,
                  6876.452,
                  8338.438,
                  9279.303)

wt_2_acetone <- c(2628.287,
                  3325.681,
                  7341.413,
                  8840.887)

wt_1_butyrate <- c(2071.046,
                   986256.5,
                   878551.25,
                   819381.063)

wt_2_butyrate <- c(2653.193,
                   659695.813,
                   817061,
                   864564.438)

wt_1_butanol <- c(67.135,
                  48937.234,
                  53842.66,
                  49924.91)

wt_2_butanol <- c(55.488,
                  28052.523,
                  46640.48,
                  45514.766)

"""Spo0a K/O"""

spo_1_glucose <- c(3932539.25,
                   5310.992,
                   5061.394,
                   4596.223)

spo_2_glucose <- c(6507835,
                   5575.53,
                   6254.248,
                   5104.329)

spo_1_lactate <- c(6276.336,
                   54810.922,
                   56240.461,
                   56824.063)

spo_2_lactate <- c(10248.608,
                   48399.539,
                   70869.555,
                   61361.832)

spo_1_acetate <- c(1119.73,
                   273393.125,
                   272893.438,
                   269889.031)

spo_2_acetate <- c(1698.856,
                   279846.75,
                   393417.5,
                   332669.031)

spo_1_ethanol <- c(2337.536,
                   13239.406,
                   13597.095,
                   14632.833)

spo_2_ethanol <- c(3138.845,
                   13320.932,
                   18812.432,
                   16294.439)

spo_1_acetone <- c(2105.102,
                   NULL,
                   5205.988,
                   6961.793)

spo_2_acetone <- c(3363.468,
                   2946.898,
                   7407.086,
                   7605.469)

spo_1_butyrate <- c(1691.056,
                    897301.688,
                    886865.938,
                    877377)

spo_2_butyrate <- c(3557,
                    900823.875,
                    1271081,
                    1070276.25)

spo_1_butanol <- c(24.837,
                   61.769,
                   26.066,
                   451.47)

spo_2_butanol <- c(10.664,
                   24.547,
                   215.879,
                   1165.873)

""" get the averages of each """

wt_glucose <- rowMeans(cbind(wt_1_glucose, wt_2_glucose))
wt_acetate <- rowMeans(cbind(wt_1_acetate, wt_2_acetate))
wt_lactate <- rowMeans(cbind(wt_1_lactate, wt_2_lactate))
wt_ethanol <- rowMeans(cbind(wt_1_ethanol, wt_2_ethanol)) 
wt_acetone <- rowMeans(cbind(wt_1_acetone, wt_2_acetone))
wt_butyrate <- rowMeans(cbind(wt_1_butyrate, wt_2_butyrate))
wt_butanol <- rowMeans(cbind(wt_1_butanol, wt_2_butanol))

spo_glucose <- rowMeans(cbind(spo_1_glucose, spo_2_glucose))
spo_acetate <- rowMeans(cbind(spo_1_acetate, spo_2_acetate))
spo_lactate <- rowMeans(cbind(spo_1_lactate, spo_2_lactate))
spo_ethanol <- rowMeans(cbind(spo_1_ethanol, spo_2_ethanol)) 
spo_acetone <- rowMeans(cbind(spo_1_acetone, spo_2_acetone))
spo_butyrate <- rowMeans(cbind(spo_1_butyrate, spo_2_butyrate))
spo_butanol <- rowMeans(cbind(spo_1_butanol, spo_2_butanol))

""" Calculate the concentration of reads based on  linear regression from the standards """
wt_glucose_concs <- (wt_glucose[]-glucose_gradient[["coefficients"]][["(Intercept)"]]) / glucose_gradient[["coefficients"]][["glucose_concentrations"]]
wt_acetate_concs <- (wt_acetate[]-acetate_gradient[["coefficients"]][["(Intercept)"]]) / acetate_gradient[["coefficients"]][["mixed_concentrations"]]
wt_lactate_concs <- (wt_lactate[]-lactate_gradient[["coefficients"]][["(Intercept)"]]) / lactate_gradient[["coefficients"]][["mixed_concentrations"]]
wt_ethanol_concs <- (wt_ethanol[]-ethanol_gradient[["coefficients"]][["(Intercept)"]]) / ethanol_gradient[["coefficients"]][["mixed_concentrations"]]
wt_acetone_concs <- (wt_acetone[]-acetone_gradient[["coefficients"]][["(Intercept)"]]) / acetone_gradient[["coefficients"]][["mixed_concentrations"]]
wt_butyrate_concs <- (wt_butyrate[]-butyrate_gradient[["coefficients"]][["(Intercept)"]]) / butyrate_gradient[["coefficients"]][["mixed_concentrations"]]
wt_butanol_concs <- (wt_butanol[]-butanol_gradient[["coefficients"]][["(Intercept)"]]) / butanol_gradient[["coefficients"]][["mixed_concentrations"]]

spo_glucose_concs <- (spo_glucose[]-glucose_gradient[["coefficients"]][["(Intercept)"]]) / glucose_gradient[["coefficients"]][["glucose_concentrations"]]
spo_acetate_concs <- (spo_acetate[]-acetate_gradient[["coefficients"]][["(Intercept)"]]) / acetate_gradient[["coefficients"]][["mixed_concentrations"]]
spo_lactate_concs <- (spo_lactate[]-lactate_gradient[["coefficients"]][["(Intercept)"]]) / lactate_gradient[["coefficients"]][["mixed_concentrations"]]
spo_ethanol_concs <- (spo_ethanol[]-ethanol_gradient[["coefficients"]][["(Intercept)"]]) / ethanol_gradient[["coefficients"]][["mixed_concentrations"]]
spo_acetone_concs <- (spo_acetone[]-acetone_gradient[["coefficients"]][["(Intercept)"]]) / acetone_gradient[["coefficients"]][["mixed_concentrations"]]
spo_butyrate_concs <- (spo_butyrate[]-butyrate_gradient[["coefficients"]][["(Intercept)"]]) / butyrate_gradient[["coefficients"]][["mixed_concentrations"]]
spo_butanol_concs <- (spo_butanol[]-butanol_gradient[["coefficients"]][["(Intercept)"]]) / butanol_gradient[["coefficients"]][["mixed_concentrations"]]


"""combine the wt and spo reads, make hours the row names and turn to DF for use in GG plot"""
glucose_concs <- as.data.frame(cbind(wt_glucose_concs, spo_glucose_concs))
acetate_concs <- as.data.frame(cbind(wt_acetate_concs, spo_acetate_concs))
lactate_concs <- as.data.frame(cbind(wt_lactate_concs, spo_lactate_concs))
ethanol_concs <- as.data.frame(cbind(wt_ethanol_concs, spo_ethanol_concs))
acetone_concs <- as.data.frame(cbind(wt_acetone_concs, spo_acetone_concs))
butyrate_concs <- as.data.frame(cbind(wt_butyrate_concs, spo_butyrate_concs))
butanol_concs <- as.data.frame(cbind(wt_butanol_concs, spo_butanol_concs))

"""plot the data"""
#for multiple variables need to specify each as its own independent line 

#GLUCOSE
ggplot(glucose_concs, aes(Hours)) + 
  geom_smooth(aes(y = (wt_glucose_concs), colour = "WT")) + 
  geom_smooth(aes(y = (spo_glucose_concs), colour = "Spo0A K/O")) +
  labs(
  x = "Hours",              
  y = "mM",   
  title = "Glucose",
  colour = "Strain") + 
  theme(plot.title = element_text(hjust = 0.5)) #make title central

#ACETATE
ggplot(acetate_concs, aes(Hours)) + 
  geom_smooth(aes(y = (wt_acetate_concs), colour = "WT")) + 
  geom_smooth(aes(y = (spo_acetate_concs), colour = "Spo0A K/O")) +
  labs(
    x = "Hours",              
    y = "mM",   
    title = "Acetate",
    colour = "Strain") + 
  theme(plot.title = element_text(hjust = 0.5)) #make title central

#ACETONE
ggplot(acetone_concs, aes(Hours)) + 
  geom_smooth(aes(y = (wt_acetone_concs), colour = "WT")) + 
  geom_smooth(aes(y = (spo_acetone_concs), colour = "Spo0A K/O")) +
  labs(
    x = "Hours",              
    y = "mM",   
    title = "Acetone",
    colour = "Strain") + 
  theme(plot.title = element_text(hjust = 0.5)) #make title central

#BUTANOL
ggplot(butanol_concs, aes(Hours)) + 
  geom_smooth(aes(y = (wt_butanol_concs), colour = "WT")) + 
  geom_smooth(aes(y = (spo_butanol_concs), colour = "Spo0A K/O")) +
  labs(
    x = "Hours",              
    y = "mM",   
    title = "Butanol",
    colour = "Strain") + 
  theme(plot.title = element_text(hjust = 0.5)) #make title central

#BUTYRATE
ggplot(butyrate_concs, aes(Hours)) + 
  geom_smooth(aes(y = (wt_butyrate_concs), colour = "WT")) + 
  geom_smooth(aes(y = (spo_butyrate_concs), colour = "Spo0A K/O")) +
  labs(
    x = "Hours",              
    y = "mM",   
    title = "Butyrate",
    colour = "Strain") + 
  theme(plot.title = element_text(hjust = 0.5)) #make title central

#ETHANOL
ggplot(ethanol_concs, aes(Hours)) + 
  geom_smooth(aes(y = (wt_ethanol_concs), colour = "WT")) + 
  geom_smooth(aes(y = (spo_ethanol_concs), colour = "Spo0A K/O")) +
  labs(
    x = "Hours",              
    y = "mM",   
    title = "Ethanol",
    colour = "Strain") + 
  theme(plot.title = element_text(hjust = 0.5)) #make title central

#LACTATE
ggplot(lactate_concs, aes(Hours)) + 
  geom_smooth(aes(y = (wt_lactate_concs), colour = "WT")) + 
  geom_smooth(aes(y = (spo_lactate_concs), colour = "Spo0A K/O")) +
  labs(
    x = "Hours",              
    y = "mM",   
    title = "Lactate",
    colour = "Strain") + 
  theme(plot.title = element_text(hjust = 0.5)) #make title central