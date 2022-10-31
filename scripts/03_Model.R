# Model Development ----
# 1: Create Initial Model Object ----
# See ?pkmodel

basemod <- pkmodel(numCompartments = 1,
                   absorption = "FirstOrder",
                   data = pkdata,
                   columnMap = FALSE,           #map columns after model creation
                   modelName = "basemod")

print(basemod)

#Hint:  for any model creation function (pkmodel, pkemaxmodel, pkindirectmodel, pklinearmodel, and linearmodel)
#you can use e.g., print(pkemaxmodel(columnMap=FALSE)) to see default model settings

# 2: Map Dataset Variables to Model ----
basemod <- basemod %>%
  colMapping(c(id = "ID", time = "TIME", Aa = "AMT", CObs = "CONC"))

print(basemod)

# 3: Set Initial Estimates and Refine Model Structure ----
basemod <- basemod %>%

  #Set initial parameter estimates ?fixedEffect
  fixedEffect(effect = c("tvKa", "tvV", "tvCl"), value = c(1.5, 80, 9)) %>%

  #Set initial ETA estimates ?randomEffect
  randomEffect(effect = c("nKa", "nV", "nCl"), value = c(0.1, 0.1, 0.1)) %>%

  #Switch to additive error model and set residual error estimate ?residualError
  residualError(predName = "C", errorType = "Additive", SD = 5)

#Add Covariates to the model (AGE, WT, SEX, RACE) ?addCovariate
#For the base model, we add the covariate to the model, but we do not
#assign any parameter relationships.  This allows base model diagnostics
#on possible covariate effects

basemod <- basemod %>%

  addCovariate(covariate = c("AGE"),
               type = "Continuous",
               center = "Value",
               centerValue = 45,
               #effect=NULL) %>%
               effect = c("V", "Cl")) %>%  #Would use this to assign age to Cl and V

  addCovariate(covariate = c("WT"),
               type = "Continuous",
               center = "Value",
               centerValue = 70,
               effect=NULL) %>%

  addCovariate(covariate = c("SEX"),
               type = "Categorical",
               levels = c(0,1),
               labels = c("M","F"),
               effect=NULL) %>%

  addCovariate(covariate = c("RACE"),
               type = "Categorical",
               levels = c(0,1,2,3),
               labels = c("WHITE","BLACK","ASIAN","OTHER"),
               effect=NULL) %>%

  addCovariate(covariate = c("DOSEGRP"),
               type = "Continuous",
               effect=NULL)

print(basemod)

#Use structuralParameter to add/remove eta terms, change form of error model ?structuralParameter

#Change style of structural parameter
basemod <- basemod %>%
  structuralParameter(paramName = 'Cl',
                      style = "LogNormal1")

print(basemod)

#Remove eta term from Ka
basemod <- basemod %>%
  structuralParameter(paramName = "Ka",
                      hasRandomEffect = FALSE)

print(basemod)

#Put eta term back on Ka
basemod <- basemod %>%
  structuralParameter(paramName = "Ka",
                      hasRandomEffect = TRUE)

print(basemod)

#Remove Covariate effects

basemod <- basemod %>%

  removeCovariate(covariate = c("AGE"),
                  paramName = c("Cl")) %>%

  removeCovariate(covariate = c("AGE"),
                  paramName = c("V"))

print(basemod)

# 4: Fit PK Model ----
basemodfit <- basemod %>%
  fitmodel(hostPlatform = localMPIHost)

print(basemodfit)

# Save as an RDS file so that we can open in RMarkdown and access results
saveRDS(basemod, file = file.path(basemod@modelInfo@workingDir, "basemod.RDS"))
saveRDS(basemodfit, file =  file.path(basemod@modelInfo@workingDir, "basemodfit.RDS"))

#Can extract elements from model fit object
names(basemodfit)
basemodfit$Overall
basemodfit$theta


# 5: Refine Error Model ----
# * 5.1 Copy Base Model to New Model ----
basemod2 <- copyModel(basemod,
                      acceptAllEffects = TRUE,
                      modelName = "basemod2")
# * 5.2 Switch to multiplicative error model ----
basemod2 <- basemod2 %>%
  residualError(predName = "C", errorType = "Multiplicative", SD = 0.1)

print(basemod2)

# * 5.3 Fit PK Model ----
basemod2fit <- basemod2 %>%
  fitmodel(hostPlatform = localMPIHost)

saveRDS(basemod2, file = file.path(basemod2@modelInfo@workingDir, "basemod2.RDS"))
saveRDS(basemod2fit, file =  file.path(basemod2@modelInfo@workingDir, "basemod2fit.RDS"))


# 6: Prepare and Execute a Stepwise Covariate Model Run ----
## Make a copy of the basemodel

covsearchmod <- copyModel(basemod2, modelName="covsearchmod")

## Add covariate effects to the model
covsearchmod <- covsearchmod %>%
  addCovariate(covariate = "WT", effect = c("Ka","V","Cl"), center = "Value", centerValue = 70) %>%
  addCovariate(covariate = "AGE", effect = c("Ka","V","Cl"), center = "Value", centerValue = 50) %>%
  addCovariate(covariate = "SEX", effect = c("Ka","V","Cl"),
               type = "Categorical", levels = c(0,1), labels=c("M","F")) %>%
  addCovariate(covariate = "RACE", effect = c("Ka","V","Cl"),
               type = "Categorical", levels = c(0,1,2,3), labels=c("WHITE","BLACK","ASIAN","OTHER"))

## View the model
print(covsearchmod)

## Execute the stepwise search
## For stepwise search it will improve speeds if we use parallelization
## Define a local multicore host - The "multicore" parallelMethod will send separate jobs
## to open cores, running models in parallel
localMultiCoreHost <- NlmeParallelHost(installationDirectory = Sys.getenv("INSTALLDIR"),
                                       parallelMethod = NlmeParallelMethod("multicore"),
                                       hostName = "Local_MultiCore",
                                       numCores = 4)


covsearchmodfit <- stepwiseSearch(covsearchmod, hostPlatform = localMultiCoreHost)

saveRDS(covsearchmod, file =  file.path(covsearchmod@modelInfo@workingDir, "covsearchmod.RDS"))
saveRDS(covsearchmodfit, file =  file.path(covsearchmod@modelInfo@workingDir, "covsearchmodfit.RDS"))


covsearchmodfit %>%
  filter(BestScenario == TRUE)

# Save covariate stepwise search results table
covsearchmodfit %>%
  select(-RetCode) %>%
  flextable() %>%
  bold(i = covsearchmodfit$BestScenario) %>%
  autofit() %>%
  save_as_html(path = file.path("tables", "covsearch_table.html"))


# 7: Create Final Model and Fit ----
finalmod <- copyModel(basemod2, acceptAllEffects = TRUE, modelName="finalmod")

## Add covariate effects to the model
finalmod <- finalmod %>%
  addCovariate(covariate = "WT", effect = c("Cl"), center = "Value", centerValue = 70)

print(finalmod)

## Fit final model
finalmodfit <- finalmod %>%
  fitmodel(hostPlatform = localMPIHost)

# Save model and fit as RDS
saveRDS(finalmod, file = file.path(finalmod@modelInfo@workingDir, "finalmod.RDS"))
saveRDS(finalmodfit, file =  file.path(finalmod@modelInfo@workingDir, "finalmodfit.RDS"))

## Save Overall Tables to HTML
modelfits <- list(basemod=basemodfit,
                  basemod2=basemod2fit,
                  finalmod=finalmodfit)

lapply(seq_along(modelfits), function(i) {
  table_name <- paste(names(modelfits)[[i]], "overall", "table.html", sep = "_")
  modelfits[[i]]$Overall %>%
    select(-c(Scenario, RetCode)) %>%
    flextable() %>%
    save_as_html(path = file.path("tables", table_name))
  print(paste(table_name, "saved"))
})

# 8: Create Visual Run Record: ----

covariatesearch_label <- covsearchmodfit %>%
  dplyr::filter(BestScenario) %>%
  mutate(`-2LL` = round(`-2LL`, 2)) %>%
  dplyr::select(Scenario, `-2LL`) %>%
  paste0(paste0(names(.), ": "), ., collapse = "\n")

models_ofv <- c(basemod = basemodfit$Overall$`-2LL`,
                basemod2 = basemod2fit$Overall$`-2LL`,
                finalmod = finalmodfit$Overall$`-2LL`
)
model_labels <-
  paste0(paste("Model:", names(models_ofv)), "\n-2LL: ", round(models_ofv, 2))

model_labels <- append(model_labels, covariatesearch_label, after = 2)

overall_tables_files <- paste0(
  "./tables/",
  c(
    "basemod_overall_table.html",
    "basemod2_overall_table.html",
    "covsearch_table.html",
    "finalmod_overall_table.html"
  )
)

overall_tables_html <- sapply(overall_tables_files, function(x){
  paste0(readLines(x), collapse = "")
})

nodes <- data.frame(id = 1:4,
                    label = model_labels,
                    shape = c("square", "square", "triangle", "star"),
                    title = overall_tables_html, #html table in tooltip
                    color = c("grey", "blue", "orange", "green"),
                    font.size = 22,
                    shadow = c(FALSE, FALSE, FALSE, TRUE))

edges <- data.frame(from = c(1, 2, 2),
                    to = c(2, 3, 4))

library(visNetwork)
run_record <- visNetwork(nodes, edges, width = "100%") %>%
  visInteraction() %>%
  visEdges(arrows = "to") %>%
  visHierarchicalLayout()

visSave(run_record, file = "./run_record/run_record.html")

