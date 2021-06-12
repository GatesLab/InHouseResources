

##path specific sensitivity calculation

#########prep###########
#first reorder the output files 
#the default would be 1, 100, 101, etc, which does not match the order they were generated
c_60_out <- c1_60_out[names(c1_60_out)[order(nchar(names(c1_60_out)),names(c1_60_out))]]
c_200_out <- c1_200_out[names(c1_200_out)[order(nchar(names(c1_200_out)),names(c1_200_out))]]
c_1000_out <- c1_1000_out[names(c1_1000_out)[order(nchar(names(c1_1000_out)),names(c1_1000_out))]]


#read in DGM
c_60 <- readRDS('/Users/LanLuo/OneDrive - University of North Carolina at Chapel Hill/hybrid_simdata_output/c1_60.RDS')
c_200 <- readRDS('/Users/LanLuo/OneDrive - University of North Carolina at Chapel Hill/hybrid_simdata_output/c1_200.RDS')
c_1000 <- readRDS('/Users/LanLuo/OneDrive - University of North Carolina at Chapel Hill/hybrid_simdata_output/c1_1000.RDS')

est_mats     <- list()
psi_mats     <- list()
true_mats    <- list()
for(p in 1:3){
  est_mats[[1]] <- lapply(c_60_out, function(x){x$path_est_mats})
  est_mats[[2]] <- lapply(c_200_out, function(x){x$path_est_mats})
  est_mats[[3]] <- lapply(c_1000_out, function(x){x$path_est_mats})
  psi_mats[[1]] <- lapply(c_60_out, function(x){x$psi})
  psi_mats[[2]] <- lapply(c_200_out, function(x){x$psi})
  psi_mats[[3]] <- lapply(c_1000_out, function(x){x$psi})
  true_mats[[1]] <- c_60$Amatrix
  true_mats[[2]] <- c_200$Amatrix
  true_mats[[3]] <- c_1000$Amatrix
}

est_mats_noNA     <- list()
psi_mats_noNA    <- list()
true_mats_noNA    <- list()

for(condition in 1:3){
  est_mats_noNA[[condition]] <- list()
  psi_mats_noNA[[condition]] <- list()
  true_mats_noNA[[condition]] <- list()
  for(i in 1:100){
    est_mats_noNA[[condition]][[i]] <- list()
    psi_mats_noNA[[condition]][[i]] <- list()
    true_mats_noNA[[condition]][[i]] <- list()
    for(j in 1:100){
      if(!is.na(est_mats[[condition]][[i]][[j]])){
        est_mats_noNA[[condition]][[i]][[j]] <- est_mats[[condition]][[i]][[j]]
        psi_mats_noNA[[condition]][[i]][[j]] <- psi_mats[[condition]][[i]][[j]]
        true_mats_noNA[[condition]][[i]][[j]] <- true_mats[[condition]][[i]][[j]]
      }
      else{
        est_mats_noNA[[condition]][[i]][[j]] <- NULL
        psi_mats_noNA[[condition]][[i]][[j]] <- NULL
        true_mats_noNA[[condition]][[i]][[j]] <- NULL
      }
    }
  }
}
library(rlist)
est_mats_noNA <- list.clean(est_mats_noNA,recursive = T)
psi_mats_noNA <- list.clean(psi_mats_noNA,recursive = T)
true_mats_noNA <- list.clean(true_mats_noNA,recursive = T)

##direction sensitivity
dir_sens_table <- matrix(0,3,6)
colnames(dir_sens_table) <- c('A21','A32','A54','A79','A89','Psi210')
rownames(dir_sens_table) <- c('60','200','1000')
pres_sens_table <- matrix(0,3,6)
colnames(pres_sens_table) <- c('A21','A32','A54','A79','A89','Psi210')
rownames(pres_sens_table) <- c('60','200','1000')
for(condition in 1:3){
  for(i in 1:length(est_mats_noNA[[condition]])){
    for(j in 1:length(true_mats_noNA[[condition]][[i]])){
      if(est_mats_noNA[[condition]][[i]][[j]][2,11]!=0){
        dir_sens_table[condition,1] <- dir_sens_table[condition,1]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][3,12]!=0){
        dir_sens_table[condition,2] <- dir_sens_table[condition,2]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][5,14]!=0){
        dir_sens_table[condition,3] <- dir_sens_table[condition,3]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][7,19]!=0){
        dir_sens_table[condition,4] <- dir_sens_table[condition,4]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][8,19]!=0){
        dir_sens_table[condition,5] <- dir_sens_table[condition,5]+1
      }
      if(psi_mats_noNA[[condition]][[i]][[j]][20,12]!=0){
        dir_sens_table[condition,6] <- dir_sens_table[condition,6]+1
      }
    }
  }
}

for(condition in 1:3){
  for(i in 1:length(psi_mats_noNA[[condition]])){
    for(j in 1:length(psi_mats_noNA[[condition]][[i]])){
      if(est_mats_noNA[[condition]][[i]][[j]][2,11]!=0 | est_mats_noNA[[condition]][[i]][[j]][1,12]!=0 | psi_mats_noNA[[condition]][[i]][[j]][12,11]!=0){
        pres_sens_table[condition,1] <- pres_sens_table[condition,1]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][3,12]!=0 | est_mats_noNA[[condition]][[i]][[j]][2,13]!=0 | psi_mats_noNA[[condition]][[i]][[j]][13,12]!=0){
        pres_sens_table[condition,2] <- pres_sens_table[condition,2]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][5,14]!=0 | est_mats_noNA[[condition]][[i]][[j]][4,15]!=0 | psi_mats_noNA[[condition]][[i]][[j]][15,14]!=0){
        pres_sens_table[condition,3] <- pres_sens_table[condition,3]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][7,19]!=0 | est_mats_noNA[[condition]][[i]][[j]][9,17]!=0 | psi_mats_noNA[[condition]][[i]][[j]][17,19]!=0){
        pres_sens_table[condition,4] <- pres_sens_table[condition,4]+1
      }
      if(est_mats_noNA[[condition]][[i]][[j]][8,19]!=0 | est_mats_noNA[[condition]][[i]][[j]][9,18]!=0 | psi_mats_noNA[[condition]][[i]][[j]][18,19]!=0){
        pres_sens_table[condition,5] <- pres_sens_table[condition,5]+1
      }
      if(psi_mats_noNA[[condition]][[i]][[j]][20,12]!=0 | est_mats_noNA[[condition]][[i]][[j]][2,20]!=0 | est_mats_noNA[[condition]][[i]][[j]][10,12]!=0){
        pres_sens_table[condition,6] <- pres_sens_table[condition,6]+1
      }
    }
  }
}
##figure 1 table
fig1table <- dir_sens_table
fig1table[,6] <- 0
colnames(fig1table)[6] <- 'A210+A102'
for(condition in 1:3){
  for(i in 1:length(est_mats_noNA[[condition]])){
    for(j in 1:length(true_mats_noNA[[condition]][[i]])){
      if(est_mats_noNA[[condition]][[i]][[j]][2,20]!=0  | est_mats_noNA[[condition]][[i]][[j]][10,12]!=0 ){
        fig1table[condition,6] <- fig1table[condition,6]+1
      }
    }
  }
}
##figure 2 table
fig2table <- dir_sens_table
fig2table[,1:5] <- 0
for(condition in 1:3){
  for(i in 1:length(est_mats_noNA[[condition]])){
    for(j in 1:length(true_mats_noNA[[condition]][[i]])){
      if(psi_mats_noNA[[condition]][[i]][[j]][12,11]!=0){
        fig2table[condition,1] <- fig2table[condition,1]+1
      }
      if(psi_mats_noNA[[condition]][[i]][[j]][13,12]!=0){
        fig2table[condition,2] <- fig2table[condition,2]+1
      }
      if(psi_mats_noNA[[condition]][[i]][[j]][15,14]!=0){
        fig2table[condition,3] <- fig2table[condition,3]+1
      }
      if(psi_mats_noNA[[condition]][[i]][[j]][17,19]!=0){
        fig2table[condition,4] <- fig2table[condition,4]+1
      }
      if(psi_mats_noNA[[condition]][[i]][[j]][18,19]!=0){
        fig2table[condition,5] <- fig2table[condition,5]+1
      }
    }
  }
}

##fig 3 table
AmatrixTrans <- function(Amatrix){
  for(p in 1:length(Amatrix)){
    Amatrix[[p]][!Amatrix[[p]]==0] <- 1
  }
  return(Amatrix)
}
add <- function(x) Reduce("+", x)

est_mats_noNA_sum <- list()
psi_mats_noNA_sum <- list()
est_mats_noNA_sum[[1]] <- lapply(est_mats_noNA[[1]], AmatrixTrans)
est_mats_noNA_sum[[2]] <- lapply(est_mats_noNA[[2]], AmatrixTrans)
est_mats_noNA_sum[[3]] <- lapply(est_mats_noNA[[3]], AmatrixTrans)
psi_mats_noNA_sum[[1]] <- lapply(psi_mats_noNA[[1]], AmatrixTrans)
psi_mats_noNA_sum[[2]] <- lapply(psi_mats_noNA[[2]], AmatrixTrans)
psi_mats_noNA_sum[[3]] <- lapply(psi_mats_noNA[[3]], AmatrixTrans)

##add up each repetition 
A_group <- list()
Psi_group <- list()
for(p in 1:3){
  A_group[[p]] <- lapply(est_mats_noNA_sum[[p]], add)
  Psi_group[[p]] <- lapply(psi_mats_noNA_sum[[p]], add)
}

#fig3
fig3_table <- fig4_table <- matrix(0, 3, 6)
rownames(fig3_table) <- rownames(fig4_table) <- c('60','200','1000')
colnames(fig3_table) <- c('A21','A32','A54','A79','A89','A210+A102')
for(condition in 1:3){
  for(i in 1:length(A_group[[condition]])){
    if(A_group[[condition]][[i]][2,11]>=75){
      fig3_table[condition,1] <- fig3_table[condition,1]+1
      }
    if(A_group[[condition]][[i]][3,12]>=75){
      fig3_table[condition,2] <- fig3_table[condition,2]+1
    }
    if(A_group[[condition]][[i]][5,14]>=75){
      fig3_table[condition,3] <- fig3_table[condition,3]+1
    }
    if(A_group[[condition]][[i]][7,19]>=75){
      fig3_table[condition,4] <- fig3_table[condition,4]+1
    }
    if(A_group[[condition]][[i]][8,19]>=75){
      fig3_table[condition,5] <- fig3_table[condition,5]+1
    }
    if(A_group[[condition]][[i]][2,20]>=75 | A_group[[condition]][[i]][10,12]>=75){
      fig3_table[condition,6] <- fig3_table[condition,6]+1
    }
  }
}
#fig4 table
colnames(fig4_table) <- c('psi21','psi32','psi54','psi79','psi89','psi210')
for(condition in 1:3){
  for(i in 1:length(Psi_group[[condition]])){
    if(Psi_group[[condition]][[i]][12,11]>=75){
      fig4_table[condition,1] <- fig4_table[condition,1]+1
    }
    if(Psi_group[[condition]][[i]][13,12]>=75){
      fig4_table[condition,2] <- fig4_table[condition,2]+1
    }
    if(Psi_group[[condition]][[i]][15,14]>=75){
      fig4_table[condition,3] <- fig4_table[condition,3]+1
    }
    if(Psi_group[[condition]][[i]][17,19]>=75){
      fig4_table[condition,4] <- fig4_table[condition,4]+1
    }
    if(Psi_group[[condition]][[i]][18,19]>=75){
      fig4_table[condition,5] <- fig4_table[condition,5]+1
    }
    if(Psi_group[[condition]][[i]][12,20]>=75){
      fig4_table[condition,6] <- fig4_table[condition,6]+1
    }
  }
}
##plotting
melt_fig1table <- melt(fig1table)
melt_fig1table$Var1 <- factor(melt_fig1table$Var1, levels = c(60, 200, 1000))
fig1_plot <- ggplot(melt_fig1table,aes(Var1,value,group = Var2, color = Var2)) + 
  geom_line() + 
  geom_point() +
  #theme(legend.position = c(0.8, 0.5)) +
  #ggtitle("Plot of total number of path recall \n - directed paths") +
  ylab("Number of path recovered") +
  xlab("Number of time points") +
  ylim(0,10000) +
  scale_color_discrete(name="Relations", labels=c(expression(a[paste(2,",",1)]), expression(a[paste(3,",",2)]), 
                                                  expression(a[paste(5,",",4)]), expression(a[paste(7,",",9)]),
                                                  expression(a[paste(8,",",9)]),
                                                  expression(paste(a[paste(2,",",10)],"+",a[paste(10,",",2)]))
                                                                     )) 
# +
#   theme(
#     legend.position = c(0.75, 0.55)
#   )
  #theme(legend.title = element_text( size=5), legend.text=element_text(size=5))

melt_fig2table <- melt(fig2table)
melt_fig2table$Var1 <- factor(melt_fig2table$Var1, levels = c(60, 200, 1000))
fig2_plot <- ggplot(melt_fig2table,aes(Var1,value,group = Var2, color = Var2)) + 
  geom_line() + 
  geom_point() +
  #theme(legend.position = c(0.8, 0.5)) +
  #ggtitle("Plot of total number of path recall \n - directed paths") +
  ylab("Number of path recovered") +
  xlab("Number of time points") +
  ylim(0,10000) +
  scale_color_discrete(name="Relations", labels=c(expression(psi[paste(2,",",1)]), expression(psi[paste(3,",",2)]), 
                                                  expression(psi[paste(5,",",4)]), expression(psi[paste(7,",",9)]),
                                                  expression(psi[paste(8,",",9)]),
                                                  expression(psi[paste(2,",",10)])
  )) 

melt_fig3table <- melt(fig3_table)
melt_fig3table$Var1 <- factor(melt_fig3table$Var1, levels = c(60, 200, 1000))
fig3_plot <- ggplot(melt_fig3table,aes(Var1,value,group = Var2, color = Var2)) + 
  geom_line() + 
  geom_point() +
  #theme(legend.position = c(0.8, 0.5)) +
  #ggtitle("Plot of total number of path recall \n - directed paths") +
  ylab("Number of path recovered at the group level") +
  xlab("Number of time points") +
  ylim(0,100) +
  scale_color_discrete(name="Relations", labels=c(expression(a[paste(2,",",1)]), expression(a[paste(3,",",2)]), 
                                                  expression(a[paste(5,",",4)]), expression(a[paste(7,",",9)]),
                                                  expression(a[paste(8,",",9)]),
                                                  expression(paste(a[paste(2,",",10)],"+",a[paste(10,",",2)]))
  )) 

melt_fig4table <- melt(fig4_table)
melt_fig4table$Var1 <- factor(melt_fig4table$Var1, levels = c(60, 200, 1000))
fig4_plot <- ggplot(melt_fig4table,aes(Var1,value,group = Var2, color = Var2)) + 
  geom_line() + 
  geom_point() +
  #theme(legend.position = c(0.8, 0.5)) +
  #ggtitle("Plot of total number of path recall \n - directed paths") +
  ylab("Number of path recovered at the group level") +
  xlab("Number of time points") +
  ylim(0,100) +
  scale_color_discrete(name="Relations", labels=c(expression(psi[paste(2,",",1)]), expression(psi[paste(3,",",2)]), 
                                                  expression(psi[paste(5,",",4)]), expression(psi[paste(7,",",9)]),
                                                  expression(psi[paste(8,",",9)]),
                                                  expression(psi[paste(2,",",10)])
  )) 
