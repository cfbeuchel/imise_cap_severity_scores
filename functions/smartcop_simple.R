smartcop_simple <- function(
  ID = NULL,
  BPSysMin = NULL,
  RespRateMin = NULL,
  RespRateMax = NULL,
  HeartRateMin = NULL,
  HeartRateMax = NULL,
  Confusion = NULL,
  GCS = NULL,
  ArtpH = NULL,
  Alb = NULL,
  Age = NULL,
  PaO2 = NULL,
  MultLobXRay = NULL
){
  
  # ID <- smartcop$input$patstuid
  # BPSysMin <- smartcop$input$sysbp.min_d0
  # RespRateMin <- smartcop$input$afrq.min_d0
  # RespRateMax <- smartcop$input$afrq.max_d0
  # HeartRateMin <- smartcop$input$hfrq.min_d0
  # HeartRateMax <- smartcop$input$hfrq.max_d0
  # Confusion <- smartcop$input$verwirrt_d0
  # GCS <- smartcop$input$gcs_d0
  # ArtpH <- smartcop$input$art.ph.min_d0
  # Alb <- smartcop$input$alb_d0
  # Age <- smartcop$input$age
  # PaO2 <- smartcop$input$oxyIndex.min_d0
  # MultLobXRay <- smartcop$input$multl_inf_d0
  
  dat <- data.table(
    id = ID,
    bpsys = BPSysMin,
    rrmin = RespRateMin,
    rrmax = RespRateMax,
    hrrmin = HeartRateMin,
    hrmax = HeartRateMax,
    conf = Confusion,
    gcs = GCS,
    artph = ArtpH,
    alb = Alb,
    age = Age,
    pao2 = PaO2,
    multl = MultLobXRay
  )
  
  
  
  
  
  stopifnot(anyDuplicated(dat[, id]) == 0)
  setDF(dat)
  sbp <- dat$bpsys
  myFilt <- is.na(sbp)
  sbp[myFilt] <- dat$bpsys[myFilt]
  SBP.p <- as.numeric(sbp < 90) * 2
  mi <- dat$multl
  myFilt <- is.na(mi)
  mi[myFilt] <- dat$multl[myFilt]
  MI.p <- as.numeric(mi)
  alb <- dat$alb
  Alb.p <- as.numeric((alb/10) < 3.5)
  af <- pmax(dat$rrmin, dat$rrmax, na.rm = T)
  myFilt <- is.na(af)
  dummy <- pmax(dat$rrmin, dat$rrmax, na.rm = T)
  af[myFilt] <- dummy[myFilt]
  AF.p = ifelse(dat$age <= 50, as.numeric(af >= 25), as.numeric(af >= 
                                                                  30))
  hrf <- pmax(dat$hrrmin, dat$hrmax, na.rm = T)
  myFilt <- is.na(hrf)
  dummy <- pmax(dat$hrmin, dat$hrmax, na.rm = T)
  hrf[myFilt] <- dummy[myFilt]
  HRF.p <- as.numeric(hrf >= 125)
  verwirrt <- dat$conf
  myFilt <- is.na(verwirrt)
  verwirrt[myFilt] <- dat$conf[myFilt]
  verwirrt[is.na(verwirrt)] <- 0
  MS.p <- as.numeric(verwirrt | (dat$gcs < 15))
  oxiIndex <- dat$pao2
  O2.p = ifelse(dat$age <= 50, as.numeric(oxiIndex < 333) * 
                  2, as.numeric(oxiIndex < 250) * 2)
  aph <- dat$artph
  myFilt <- is.na(aph)
  aph[myFilt] <- dat$artph[myFilt]
  apH.p <- as.numeric(aph < 7.35) * 2
  dummy <- cbind(SBP.p, MI.p, Alb.p, AF.p, HRF.p, MS.p, O2.p, 
                 apH.p)
  smart.COP <- apply(dummy, 1, function(x) sum(x, na.rm = T))
  res = data.table(smartCOP = smart.COP)
  res$PATSTUID = dat$id
  return(res)
  }