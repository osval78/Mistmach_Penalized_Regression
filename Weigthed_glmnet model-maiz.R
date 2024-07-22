
rm(list = ls())
library(SKM)
library(glmnet)
#make this example reproducible
set.seed(1)                  #FIJAMOS SEMILLA PARA OBTENER SIEMPRE LA MISMA MUESTRA
Summary_All=data.frame()      #INICIALIZAMOS DATA FRAME A UTILIZAR POSTERIORMENTE, ASI CADA QUE EJECUTEMOS ESTARA VACIO

#CARGAMOS LOS DATOS DEL CONJUNTO NORWAY
data <- load("Maize-Part4.RData", verbose = TRUE)
ls()
All_data_summary_RR=data.frame()
All_data_summary_LR=data.frame()
All_data_summary_ENR=data.frame()
All_data_summary_WRR=data.frame()
All_data_summary_WLR=data.frame()
All_data_summary_WENR=data.frame()


Envs = unique(Pheno$Loc)
for (e in 1:length(Envs)) {
#e=1
cat("*** Env:", e, " ***\n")
env = Envs[e]
pos_e=which(Pheno$Loc %in% env)
Pheno1= Pheno[pos_e,]
Pheno1= droplevels(Pheno1)
Pheno1$GID = as.character(Pheno1$GID)
coincidencias1 <- which(rownames(Geno) %in% Pheno1$GID)
coincidencias2<- which(rownames(Markers) %in% Pheno1$GID)
Geno_1 <- Geno[coincidencias1,coincidencias1]
Markers_e=Markers[coincidencias2,]
dim(Markers_e)

Pheno1$GID = as.factor(Pheno1$GID)


X=Markers_e    #UNIR LAS MATRICES POR COLUMNAS, CADA COLUMNA SE USARA PARA LA PREDICCION
#head(Pheno)
Trait_names=colnames(Pheno1)[4:ncol(Pheno1)]  #ELIMINAR COLUMNAS QUE NO SON DE INTERES
#Trait_names
All_summary_RR=data.frame()
All_summary_LR=data.frame()
All_summary_ENR=data.frame()
All_summary_WRR=data.frame()
All_summary_WLR=data.frame()
All_summary_WENR=data.frame()

for (t in 1:2){
#t=1
Trait_t=Trait_names[t]            #VARIABLE DE INTERES
y<- Pheno1[, 3+t]                  #VARIABLE A PREDECIR
pos_Na=which(is.na(y)==TRUE)      #DEPURACION DE DATOS FALTANTES INDICA LA POSISCION DE DATOS FALTANTES
pos_Na
if (length(pos_Na)>0) {           #SI HAY DATOS FALTANTES ASIGNARA LA MEDIANA DE LOS DATOS
  y[ pos_Na]=median(y)
} else {
  y=y
}
y_bin1=rep(0,length(y))
Fams=unique(Pheno1$Family)
Predictions_RR <- data.frame()      #PARA GUARDAR PREDICCIONES
Predictions_LR <- data.frame()
Predictions_ENR <- data.frame()
Predictions_WRR <- data.frame()      #PARA GUARDAR PREDICCIONES
Predictions_WLR <- data.frame()
Predictions_WENR <- data.frame()

 for (fa in 1:length(Fams)){     #ESCOGE UN EC Y LO USA PARA EL TESTING Y CON EL RESTO REALIZA ENTRENAMIENTO
#fa=1
cat("*** Fold:", fa, " ***\n")

tst_e=Fams[fa]                             #AMBIENTE DE PRUEBA
# tst_e
pos_test_e=which(Pheno1$Family %in% tst_e)
#QUE POSICIONES SI PERTENECE AL AMBIENTE
y_bin=y_bin1
y_bin[pos_test_e]=1
# pos_test_e
X_training <- X[-pos_test_e, ]  #CONJUNTO DE ENTRENAMIENTO EN VAR PRED
X_testing <- X[ pos_test_e, ]   #CONJUNTO DE TESTEO EN VAR PRED
y_training <- y[-pos_test_e]    #CONJUNTO DE ENTRENAMIENTO EN VAR RESPUESTA
y_testing <- y[pos_test_e]      #CONJUNTO DE TESTEO EN VAR RESPUESTA
# RR == Ridge regression
RR_model <- cv.glmnet(X_training, y_training, family='gaussian',
                      alpha=0, type.measure='mse')
Predicted_RR <- as.numeric(predict(RR_model, newx=as.matrix(X_testing),
                                     s='lambda.min', type='response'))#Aqui cambie a response
#DATA FRAME PARA GUARDAR PREDICCIONES POR AMBIENTE 
Predictions_Ridge=data.frame(Line=Pheno1$GID[pos_test_e], Fold=fa, Env=Pheno1$Family[pos_test_e], Observed=y_testing,Predicted=Predicted_RR)
Predictions_RR=rbind(Predictions_RR,Predictions_Ridge)  #CONCATENAR LAS PREDICCIONES POR AMBIENTE POR FILA
###########Weigthed Ridge Regression
RR_model_Bin <- cv.glmnet(X, y_bin, family='binomial',
                      alpha=0, type.measure='class')
init.est <- coef(RR_model_Bin, s = RR_model_Bin$lambda[floor(length(RR_model_Bin$lambda) / 2)])[-1]

WRR_model <- cv.glmnet(X_training, y_training, family='gaussian',
                      alpha=0, type.measure='mse',penalty.factor = 1 / (abs(init.est)))

Predicted_WRR <- as.numeric(predict(WRR_model, newx=as.matrix(X_testing),#Aqui cambie a response
                                    s='lambda.min', type='response', penalty.factor = 1 / (abs(init.est))))
#DATA FRAME PARA GUARDAR PREDICCIONES POR AMBIENTE 
Predictions_WRidge=data.frame(Line=Pheno1$GID[pos_test_e], Fold=fa, Env=Pheno1$Family[pos_test_e], Observed=y_testing,Predicted=Predicted_WRR)
Predictions_WRR=rbind(Predictions_WRR,Predictions_WRidge)  #CONCATENAR LAS PREDICCIONES POR AMBIENTE POR FILA

# LR ==Lasso regression
LR_model <- cv.glmnet(X_training, y_training, family='gaussian',
                      alpha=1, type.measure='mse')
Predicted_LR <- as.numeric(predict(LR_model, newx=as.matrix(X_testing),
                                   s='lambda.min', type='response'))#Aqui cambie a response
#DATA FRAME PARA GUARDAR PREDICCIONES POR AMBIENTE 
Predictions_Lasso=data.frame(Line=Pheno1$GID[pos_test_e], Fold=fa, Env=Pheno1$Family[pos_test_e], Observed=y_testing,Predicted=Predicted_LR)
Predictions_LR=rbind(Predictions_LR,Predictions_Lasso)  #CONCATENAR LAS PREDICCIONES POR AMBIENTE POR FILA
###########Weigthed Lasso Regression
###########Weigthed Ridge Regression
LR_model_Bin <- cv.glmnet(X, y_bin, family='binomial',
                          alpha=1, type.measure='class')
init.estLR <- coef(LR_model_Bin, s = LR_model_Bin$lambda[floor(length(LR_model_Bin$lambda) / 2)])[-1]
WLR_model <- cv.glmnet(X_training, y_training, family='gaussian',
                       alpha=0, type.measure='mse',penalty.factor = 1 / (abs(init.estLR)))

Predicted_WLR <- as.numeric(predict(WLR_model, newx=as.matrix(X_testing),#Aqui cambie a response
                                    s='lambda.min', type='response', penalty.factor = 1 / (abs(init.estLR))))
#DATA FRAME PARA GUARDAR PREDICCIONES POR AMBIENTE 
Predictions_WLasso=data.frame(Line=Pheno1$GID[pos_test_e], Fold=fa, Env=Pheno1$Family[pos_test_e], Observed=y_testing,Predicted=Predicted_WLR)
Predictions_WLR=rbind(Predictions_WLR,Predictions_WLasso)  #CONCATENAR LAS PREDICCIONES POR AMBIENTE POR FILA

# ENR ==Elastic Net regression
ENR_model <- cv.glmnet(X_training, y_training, family='gaussian',
                      alpha=0.5, type.measure='mse')
Predicted_ENR <- as.numeric(predict(ENR_model, newx=as.matrix(X_testing),
                                   s='lambda.min', type='response'))#Aqui cambie a response
#DATA FRAME PARA GUARDAR PREDICCIONES POR AMBIENTE 
Predictions_ENet=data.frame(Line=Pheno1$GID[pos_test_e], Fold=fa, Env=Pheno1$Family[pos_test_e], Observed=y_testing,Predicted=Predicted_ENR)
Predictions_ENR=rbind(Predictions_ENR,Predictions_ENet)  #CONCATENAR LAS PREDICCIONES POR AMBIENTE POR FILA
###########Weigthed Lasso Regression
###########Weigthed Ridge Regression
ENR_model_Bin <- cv.glmnet(X, y_bin, family='binomial',
                          alpha=0.5, type.measure='class')
init.estENR <- coef(ENR_model_Bin , s = ENR_model_Bin $lambda[floor(length(ENR_model_Bin$lambda) / 2)])[-1]
WENR_model <- cv.glmnet(X_training, y_training, family='gaussian',
                       alpha=0, type.measure='mse',penalty.factor = 1 / (abs(init.estENR)))

Predicted_WENR <- as.numeric(predict(WENR_model, newx=as.matrix(X_testing),#Aqui a response
                                    s='lambda.min', type='response', penalty.factor = 1 / (abs(init.estENR))))
#DATA FRAME PARA GUARDAR PREDICCIONES POR AMBIENTE 
Predictions_WENet=data.frame(Line=Pheno1$GID[pos_test_e], Fold=fa, Env=Pheno1$Family[pos_test_e], Observed=y_testing,Predicted=Predicted_WENR)
Predictions_WENR=rbind(Predictions_WENR,Predictions_WENet)  #CONCATENAR LAS PREDICCIONES POR AMBIENTE POR FILA

}

Pred_Env_RR=gs_summaries(Predictions_RR)$env
Pred_Env_LR=gs_summaries(Predictions_LR)$env
Pred_Env_ENR=gs_summaries(Predictions_ENR)$env
Pred_Env_WRR=gs_summaries(Predictions_WRR)$env
Pred_Env_WLR=gs_summaries(Predictions_WLR)$env
Pred_Env_WENR=gs_summaries(Predictions_WENR)$env

All_summary_RR=rbind(All_summary_RR,data.frame(Model="Ridge", Trait_t=Trait_t, Pred_Env_RR))
All_summary_LR=rbind(All_summary_LR,data.frame(Model="Lasso", Trait_t=Trait_t, Pred_Env_LR))
All_summary_ENR=rbind(All_summary_ENR,data.frame(Model="ENet", Trait_t=Trait_t, Pred_Env_ENR))
All_summary_WRR=rbind(All_summary_WRR,data.frame(Model="WRidge", Trait_t=Trait_t, Pred_Env_WRR))
All_summary_WLR=rbind(All_summary_WLR,data.frame(Model="WLasso", Trait_t=Trait_t, Pred_Env_WLR))
All_summary_WENR=rbind(All_summary_WENR,data.frame(Model="WENet", Trait_t=Trait_t, Pred_Env_WENR))

}
All_summary_RR=cbind(Loc=Envs[e],All_summary_RR)
All_summary_LR=cbind(Loc=Envs[e],All_summary_LR)
All_summary_ENR=cbind(Loc=Envs[e],All_summary_ENR)
All_summary_WRR=cbind(Loc=Envs[e],All_summary_WRR)
All_summary_WLR=cbind(Loc=Envs[e],All_summary_WLR)
All_summary_WENR=cbind(Loc=Envs[e],All_summary_WENR)

All_data_summary_RR=rbind(All_data_summary_RR,All_summary_RR)
All_data_summary_LR=rbind(All_data_summary_LR,All_summary_LR)
All_data_summary_ENR=rbind(All_data_summary_ENR,All_summary_ENR)
All_data_summary_WRR=rbind(All_data_summary_WRR,All_summary_WRR)
All_data_summary_WLR=rbind(All_data_summary_WLR,All_summary_WLR)
All_data_summary_WENR=rbind(All_data_summary_WENR,All_summary_WENR)


}
write.csv(All_data_summary_RR,file="All_data_summary_RR2.csv")
write.csv(All_data_summary_LR,file="All_data_summary_LR2.csv")
write.csv(All_data_summary_ENR,file="All_data_summary_ENR2.csv")
write.csv(All_data_summary_WRR,file="All_data_summary_WRR2.csv")
write.csv(All_data_summary_WLR,file="All_data_summary_WLR2.csv")
write.csv(All_data_summary_WENR,file="All_data_summary_WENR2.csv")