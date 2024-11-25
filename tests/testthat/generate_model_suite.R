
makePropSigmaError <- function(model) {
  
  # 1) replace the error block
  model <- model %>% delete(Equation("IPRED")) %>%
    delete(Equation("W"))  %>%
    delete(Equation("Y"))  %>%
    delete(Equation("TVPROP_RUV")) %>%
    delete(Equation("PROP_RUV"))
  
  model <- model %>% add(Equation("CONC_ERR","CONC*(1 + EPS_PROP_RUV)"),pos=Position(ErrorRecord()))
  
  # 2) adjust the parameters
  model <- model %>% delete(Theta(name="PROP_RUV")) %>%
    delete(Sigma(name="RUV_FIX")) %>%
    add(Sigma(name="PROP_RUV", value=0.1, type="sd"))
  
  return(model)
}

addTMDDspecifics <- function(model) {
  model <- model %>% 
    add(Comment("NOTE: DOSE is in mg, TIME is in days"), pos=Position(1, after=FALSE)) %>%
    add(Equation("MW","150000", comment='Molecular weight (g/mole)'), pos=Position(1, after=TRUE))
  
  model <- model %>%
    add(Equation("CONC_mgL","MW/1000000 *CONC"),pos=Position(ErrorRecord()))
  model <- model %>%
    add(Equation("CONC_ngML","CONC_mgL*1000"),pos=Position(ErrorRecord()))
  
  return(model)
}

replaceR0ByRBSL <- function(model) {
  model <- model %>% 
    replaceAll("R0", "RBSL")
  return(model)
}

modelPostProcessing <- function(model, shortName) {
  
  # Rework error model
  model <- model %>% makePropSigmaError()
  
  # Add TMDD specifics
  if (grepl("tmdd", x=shortName)) {
    model <- model %>% addTMDDspecifics()
  }
  
  # Delete unused equation KDEG if wagner
  if (grepl("wagner", x=shortName)) {
    model <- model %>% delete(Equation("KDEG"))
  }
  
  # Special post-processing for weibull models (for use with multiple dosing)
  if (grepl("weibull", x=shortName)) {
    model <- model %>%
      replaceAll(pattern=Pattern("\\(t-TDOS\\)"), "TSUB") %>%
      add(IfStatement("TSUB < 0", Equation("TSUB", "0")), pos=Position(OdeRecord(), after=FALSE)) %>%
      add(Equation("TSUB", "t - TDOS"), pos=Position(OdeRecord(), after=FALSE))
  }
  
  return(model)
}

generatePKSuite <- function() {
  require(calvamod)
  
  pkParameters <- Parameters() %>%
    # Lag time
    add(Theta("LAG", value=1,label = 'Lag time',unit = 'h')) %>%
    
    # Bioavailability
    add(Theta("BIO", value=1,label = 'Bioavailability')) %>%
    
    # Infusion duration
    add(Theta("DUR", value=4,label = 'Duration of 0-order input',unit = 'h')) %>%
    
    # Absorptions
    add(Theta("MTT", value=10, label= 'Mean transit time',unit = 'h')) %>%
    add(Theta("KA",  value=1,  label= 'Absorption rate',unit = '1/h')) %>%
    add(Theta("GAM", value=0.5,label= 'Weibull shape parameter')) %>%
    add(Theta("FR",  value=0.5,label= 'Fraction absorbed by 1st Weibull function')) %>%
    add(Theta("KA1", value=2,  label= 'Absorption rate 1',unit = '1/h')) %>%
    add(Theta("GAM1",value=0.5,label= 'Weibull shape parameter 1')) %>%
    add(Theta("KA2", value=0.5,label= 'Absorption rate 2',unit = '1/h')) %>%
    add(Theta("GAM2",value=4,  label= 'Weibull shape parameter 2')) %>%
    add(Theta("TAU", value=5,  label= 'Weibull parameter')) %>%
    add(Theta("SH",  value=2,  label= 'Weibull shape parameter'))  %>%
    add(Theta("VMABS", value=50,label= 'Max. absorption rate')) %>%
    add(Theta("KMABS", value=50,label= 'Amount at 50% max. absorption rate', unit = 'mg')) %>%
    
    # Central volumes
    add(Theta("VC", value=10, label= 'Volume of central compartment',unit = 'L')) %>%
    
    # Peripheral volumes
    add(Theta("VP", value=40,   label= 'Volume of peripheral compartment',unit = 'L')) %>%
    add(Theta("Q", value=20  ,   label= 'Inter-compartment flow',unit = 'L/h')) %>%
    add(Theta("VP2", value=200, label= 'Volume of peripheral compartment 2',unit = 'L')) %>%
    add(Theta("Q2", value=10,    label= 'Inter-compartment flow 2',unit = 'L/h')) %>%
    
    # Elimination
    add(Theta("CL", value=3,   label= 'Clearance',unit = 'L/h')) %>%
    add(Theta("VMAX", value=20, label= 'Maximal elimination rate',unit = 'mg/h')) %>%
    add(Theta("KM", value=5 ,   label= 'Concentration at 50% max. elimination',unit = 'mg/L')) %>%
    
    # Error
    add(Theta("PROP_RUV", value=0.15,label = 'Proportional residual error')) %>%
    
    add(Omega("LAG", value=25, type="cv%")) %>%
    add(Omega("DUR", value=25, type="cv%")) %>%
    add(Omega("MTT", value=25, type="cv%")) %>%
    add(Omega("TAU", value=25, type="cv%")) %>%
    add(Omega("SH", value=10, type="cv%")) %>%
    add(Omega("KA", value=25, type="cv%")) %>%
    add(Omega("KA1", value=25, type="cv%")) %>%
    add(Omega("KA2", value=25, type="cv%")) %>%
    add(Omega("GAM", value=25, type="cv%")) %>%
    add(Omega("GAM1", value=25, type="cv%")) %>%
    add(Omega("GAM2", value=25, type="cv%")) %>%
    add(Omega("VC", value=25, type="cv%")) %>%
    add(Omega("VP", value=25, type="cv%")) %>%
    add(Omega("Q", value=25, type="cv%")) %>%
    add(Omega("CL", value=25, type="cv%")) %>%
    add(Omega("VMAX", value=25, type="cv%")) %>%
    add(Omega("KM", value=25, type="cv%"))
  
  pkConfig <- PkConfiguration(
    route = AllRoutes(),
    include_default=TRUE, include_zo_abs=FALSE
  )
  
  pks <- PkModel() %>%
    add(CalvaDataset(getPKReferenceDataset())) %>%
    add(pkParameters) %>%
    generate(config=pkConfig) %>%
    export(dest="campsis")
  
  return(pks)
}

getPKReferenceDataset <- function(infusion=FALSE) {
  if (infusion) {
    dataset <- Dataset(3) %>%
      add(Infusion(time=0, amount=1000)) %>%
      add(Observations(times=1:24)) %>%
      add(Covariate("DOSE", 1000))
  } else {
    dataset <- Dataset(3) %>%
      add(Bolus(time=0, amount=1000)) %>%
      add(Observations(times=1:24)) %>%
      add(Covariate("DOSE", 1000))
  }
  return(dataset)
}

getTmddReferenceDataset <- function(infusion=FALSE) {
  if (infusion) {
    dataset <- Dataset(3) %>%
      add(Infusion(time=0, amount=1000)) %>%
      add(Observations(times=seq(1,100,by=4))) %>%
      add(Covariate("DOSE", 1000))
  } else {
    dataset <- Dataset(3) %>%
      add(Bolus(time=0, amount=1000)) %>%
      add(Observations(times=seq(1,100,by=4))) %>%
      add(Covariate("DOSE", 1000))
  }
  return(dataset)
}

generateTmddSuite <- function() {
  require(calvamod)
  
  tmddParameters <- Parameters() %>%
    add(Theta(name="DUR", value=0.00001, fix=TRUE,label = 'Duration of 0-order input',unit = 'd')) %>% # IV bolus ZO
    add(Theta(name="BIO",  value=0.6,label = 'Bioavailability')) %>% # Bioavailability
    add(Theta(name="KA", value=0.2,  label= 'Absorption rate',unit = '1/d')) %>% # FO
    add(Theta(name="VC", value=3,    label= 'Volume of central compartment',unit = 'L')) %>% # L
    add(Theta(name="VP", value=3,    label= 'Volume of peripheral compartment',unit = 'L')) %>% # L
    add(Theta(name="Q", value=0.5,   label= 'Inter-compartment flow',unit = 'L/d')) %>% # # L/day
    add(Theta(name="CL", value=0.5,  label= 'Linear clearance',unit = 'L/d')) %>% # L/day
    add(Theta(name="R0", value=20,   label= 'Baseline target concentration',unit = 'nM')) %>% # nM
    add(Theta(name="KSYN", value=30, label= 'Rate of target production',unit = 'nM/d')) %>% # nM/day
    add(Theta(name="KDEG", value=2,  label= 'Degradation rate of free target',unit = '1/d')) %>% # 1/day
    add(Theta(name="KON", value=50,  label= 'Association rate',unit = '1/nM/d')) %>% # 1/nM/day
    add(Theta(name="KD", value=1,    label= 'Dissociation constant',unit='nM')) %>% # nM
    add(Theta(name="KSS", value=1,   label= 'Approximate dissociation constant',unit='nM')) %>% # nM
    add(Theta(name="KINT", value=1,label= 'Internalization/Degradation rate of drug-target complex', unit='1/d')) %>% # 1/day
    add(Theta(name="PROP_RUV", value=0.15,label = 'Proportional residual error')) %>%
    
    add(Omega(name="KA", value=25, type="cv%")) %>%
    add(Omega(name="VC", value=25, type="cv%")) %>%
    add(Omega(name="VP", value=25, type="cv%")) %>%
    add(Omega(name="Q",  value=25, type="cv%")) %>%
    add(Omega(name="CL", value=25, type="cv%")) %>%
    add(Omega(name="R0", value=25, type="cv%")) %>%
    add(Omega(name="KDEG", value=25, type="cv%")) %>%
    add(Omega(name="KON",  value=25, type="cv%")) %>%
    add(Omega(name="KD",   value=25, type="cv%")) %>%
    add(Omega(name="KSS",  value=25, type="cv%")) %>%
    add(Omega(name="KINT", value=25, type="cv%"))

  pkConfigTmdd <- PkConfiguration(
    route=AllRoutes(),
    include_default=FALSE
  )
  
  tmddConfig <- TmddConfiguration(pk_config=pkConfigTmdd)
  
  tmdd1 <- FullTmddModel() %>%
    add(CalvaDataset(getTmddReferenceDataset())) %>%
    add(tmddParameters) %>%
    generate(config=tmddConfig)
  
  tmdd2 <- ApproximateTmddModel() %>%
    add(CalvaDataset(getTmddReferenceDataset())) %>%
    add(tmddParameters) %>%
    generate(config=tmddConfig)
  
  tmdds <- tmdd1 %>%
    add(tmdd2) %>%
    export(dest="campsis")
  
  return(tmdds)
}

generateModelSuite <- function() {
  modelSuite <- generatePKSuite() %>%
    add(generateTmddSuite())
  return(modelSuite)
}

generateModelSuiteFiles <- function() {
  pks <- generateModelSuite() %>%
    discard3CptZoModels()
  command <- "execute"
  
  for (pk in pks@list) {
    print(pk %>% getShortName())
    pathToCtl <- generatePKForSimulation(pk)
    command <- paste0(command, " ", pathToCtl)
  }
  # Careful, execute on Windows not accepting too many models (e.g. like > 100 models)
  cat(command)
}

fixAllParameters <- function(model) {
  model@parameters@list <- model@parameters@list %>% purrr::map(.f=function(x) {
    x@fix <- TRUE
    return(x)
  })
  return(model)
}

addTable <- function(nonmem, output) {
  nonmem@table <- nonmem@table %>%
    add(KeyValue("ID")) %>%
    add(KeyValue("ARM")) %>%
    add(KeyValue("TIME")) %>%
    add(KeyValue("EVID")) %>%
    add(KeyValue("MDV")) %>%
    add(KeyValue("DV")) %>%
    add(KeyValue("AMT")) %>%
    add(KeyValue("CMT")) %>%
    add(KeyValue("DOSENO")) %>%
    add(KeyValue(output)) %>%
    add(KeyValue("FILE", "output.tab")) %>%
    add(KeyValue("ONEHEADER")) %>%
    add(KeyValue("NOAPPEND")) %>%
    add(KeyValue("NOPRINT"))
  return(nonmem)
}

generatePKForSimulation <- function(pk) {
  obj <- pk@export
  
  shortName <- pk %>% getShortName()
  
  # Campsis model with a few adjustments
  campsis <- obj@model %>%
    modelPostProcessing(shortName=shortName) %>%
    replaceR0ByRBSL()
  
  # Fix all parameters in control stream, disable IIV (ETA's passed through CAMPSIS dataset) and RUV
  nonmem <- campsis %>%
    fixAllParameters() %>%
    disable(c("IIV", "RUV")) %>%
    export(dest=calvamod::NONMEMModel(), imodel=pk, estimation=FALSE)
  
  # Add output table
  nonmem <- nonmem %>% addTable(output="CONC")
  
  final_dataset <- obj@dataset
  
  # Export dataset
  table <- final_dataset %>% export(dest="mrgsolve", model=campsis, seed=1, settings=Settings(NOCB(TRUE, "TDOS"))) %>%
    dplyr::mutate_if(is.numeric, signif) %>%
    calvamod::standardiseDataset()
  
  for (header in colnames(table)) {
    nonmem@input <- nonmem@input %>% add(KeyValue(header))
  }
  
  # Create PK folder
  pkFolder <- paste0(testFolder, "qualification/", shortName, "/")
  dir.create(file.path(pkFolder), showWarnings=FALSE)
  
  # Create NONMEM folder
  nonmemFolder <- paste0(pkFolder, "nonmem/")
  dir.create(file.path(nonmemFolder), showWarnings=FALSE)
  
  # Export NONMEM
  nonmem %>% write(paste0(nonmemFolder, "model.mod"))
  
  # Write dataset
  write.csv(table, file=paste0(nonmemFolder, "dataset.csv"), row.names=FALSE, quote=FALSE)
  
  # Export CAMPSIS
  campsisFolder <- paste0(pkFolder, "campsis/")
  campsis %>% write(campsisFolder)
  
  return(paste0(nonmemFolder, "model.mod"))
}
