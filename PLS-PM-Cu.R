library(readxl)
luces <- read_excel("G:/Mi unidad/Tesis doctorado/Ensayo luces/datasetlog.xlsx")
library(dplyr)
data<-luces %>%
  dplyr::select('dw','Cu','N','K','P','Ca','Mg','Cl','CE','As') #agrego dplyr:: para que pesque este paquete, ya que de lo contrario agarra MASS
names <- c('dw','Cu','N','K','P','Ca','Mg','Cl','CE')
data[,names] <- lapply(data[,names] , as.numeric)
str(data)
head(data)

library("plspm")
# rows of the inner model matrix
Plant = c(0,0,0,0)
Stressors = c(0,0,0,0)
Nutrients = c(1,1,0,0)
DryWeight = c(0,1,1,0)

# path matrix created by row binding
exp_path = rbind(Stressors,Nutrients,DryWeight)
# add column names (optional)
colnames(exp_path) = rownames(exp_path)
# plot the path matrix
innerplot(exp_path)
head(data)
# define list of indicators: what variables are associated with
# what latent variables
exp_blocks = list(8,2,3:7,1)

# A for reflective, B for formatives 
exp_modes = rep("A",4)

# plspm(data, path matrix, blocks, modes = NULL) 
# run plspm analysis
exp_pls = plspm(data, exp_path, exp_blocks, modes = exp_modes)
exp_pls

exp_pls_bs = plspm(data, exp_path, exp_blocks, modes = exp_modes,
                   boot.val = TRUE, br = 999)


exp_pls$inner_model
exp_pls_bs$boot$paths

# summarized results
summary(exp_pls)
summary(exp_pls_bs)
