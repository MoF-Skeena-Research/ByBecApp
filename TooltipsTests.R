library(rhandsontable)
library(htmltools)
dat <- mtcars
ttips <- 1:nrow(mtcars)
browsable(
  tagList(
    rhandsontable(dat) %>%
      hot_col(2,renderer = paste0("function(instance, td, row, col, prop, value, cellProperties) {
                              var titleLookup = ['",paste(ttips, collapse = "','"),"'];
                              if(td.hasOwnProperty('_tippy')) {td._tippy.destroy()}
                              tippy(td, {
                                content: titleLookup[row],
                              });
                              Handsontable.renderers.TextRenderer.apply(this, arguments);
                              return (td);
                            }")),
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6")
  )
)

dat <- fread("ForestHealth_Download.csv")
pests <- unique(dat$pest)
dbDat <- dbGetQuery(sppDb,paste0("select * from forhealth where pest IN ('",
                    paste(pests,collapse = "','"),"') and region = 'BC'"))
dbDat <- as.data.table(dbDat)
dbDat[dat, new := i.hazard_update, on = c("bgc","treecode","pest")]
datNew <- dbDat[hazard_update != new,]
datNew[,mod := "Dragon"]
datNew[,comb := paste0("('",bgc,"','",treecode,"','",pest,"','",hazard_update,"','",mod,"')")]
dat <- paste(datNew$comb,collapse = ",")
dbExecute(sppDb,paste0("UPDATE forhealth
               SET hazard_update = new.hazard_update,
               mod = new.mod
               FROM (values ",dat,") 
               AS new(bgc,treecode,pest,hazard_update,mod)
               WHERE (forhealth.bgc = new.bgc AND forhealth.treecode = new.treecode AND forhealth.pest = new.pest)"))
