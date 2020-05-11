scap_simple <- function(
  ID = NULL,
  BPSysMin = NULL,
  RespRateMin = NULL,
  RespRateMax = NULL,
  Confusion = NULL,
  GCS = NULL,
  ArtpH = NULL,
  BUN = NULL,
  Age = NULL,
  PaO2 = NULL,
  MultLobXRay = NULL
){
  
  # ID <- scap$input$patstuid
  # BPSysMin <- scap$input$sysbp.min_d0
  # RespRateMin <- scap$input$afrq.min_d0
  # RespRateMax <- scap$input$afrq.max_d0
  # Confusion <- scap$input$verwirrt_d0
  # GCS <- scap$input$gcs_d0
  # ArtpH <- scap$input$art.ph.min_d0
  # BUN <- scap$input$bun_d0
  # Age <- scap$input$age
  # PaO2 <- scap$input$oxyIndex.min_d0
  # MultLobXRay <- scap$input$multl_inf_d0
  
  dat <- data.table(
    id = ID,
    bpsys = BPSysMin,
    rrmin = RespRateMin,
    rrmax = RespRateMax,
    conf = Confusion,
    gcs = GCS,
    artph = ArtpH,
    bun = BUN,
    age = Age,
    pao2 = PaO2,
    multl = MultLobXRay
  )
  
  stopifnot(anyDuplicated(dat[, id]) == 0)
  setDF(dat)
  aph <- dat$artph
  myFilt <- is.na(aph)
  aph[myFilt] <- dat$artph[myFilt]
  apH.p <- as.numeric(aph < 7.3) * 13
  sbp <- dat$bpsys
  myFilt <- is.na(sbp)
  sbp[myFilt] <- dat$bpsys[myFilt]
  SBP.p <- as.numeric(sbp < 90) * 11
  af <- pmax(dat$rrmin, dat$rrmax, na.rm = T)
  myFilt <- is.na(af)
  dummy <- pmax(dat$rrmin, dat$rrmax, na.rm = T)
  af[myFilt] <- dummy[myFilt]
  AF.p <- as.numeric(af > 30) * 9
  bhss <- dat$bun
  myFilt <- is.na(bhss)
  bhss[myFilt] <- dat$bun[myFilt]
  BHSS.p <- as.numeric((bhss/0.357) > 30) * 5
  verwirrt <- dat$conf
  myFilt <- is.na(verwirrt)
  verwirrt[myFilt] <- dat$conf[myFilt]
  verwirrt[is.na(verwirrt)] <- 0
  MS.p <- as.numeric(verwirrt | (dat$gcs < 15)) * 5
  oxiIndex <- dat$pao2
  O2.p <- as.numeric(oxiIndex < 250) * 6
  Age.p <- as.numeric(dat$age >= 80) * 5
  mi <- dat$multl
  myFilt <- is.na(mi)
  mi[myFilt] <- dat$multl[myFilt]
  MI.p <- as.numeric(mi) * 5
  dummy <- cbind(apH.p, SBP.p, AF.p, BHSS.p, MS.p, O2.p, Age.p, 
                 MI.p)
  SCAP <- apply(dummy, 1, function(x) sum(x, na.rm = T))
  res = data.table(SCAP)
  res$PATSTUID = dat$id
  return(res)
  
}