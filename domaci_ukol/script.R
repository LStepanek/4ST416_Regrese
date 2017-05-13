###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(c(
            "xtable",
            "openxlsx"
        ),
        function(package){
            
            if(!(package %in% rownames(installed.packages()))){
    
                install.packages(
                    package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                )
        
            }
  
            library(package, character.only = TRUE)
            
        }
    )
)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R ----------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip") 


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!"script.R" %in% dir()){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím posložky pracovní složky ------------------------------------------

setwd(mother_working_directory)

for(my_subdirectory in c("vstupy", "vystupy")){
    
    if(!file.exists(my_subdirectory)){

        dir.create(file.path(
        
            mother_working_directory, my_subdirectory
            
        ))
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################

## loaduji data ---------------------------------------------------------------

my_data <- read.table(
    
    file = "http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P155b.txt",
    header = TRUE,
    sep = "\t"
        
)


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím pro jistotu offline kopii dat a ukládám ji ------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))

#### nejdříve plaintextová kopie dat ------------------------------------------

write.table(
    x = my_data,
    file = "P155b.txt",
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE
)


#### a nyní Excelovou kopii dat -----------------------------------------------

###### vytvářím sešit ---------------------------------------------------------
    
addWorksheet(
    wb = data_backup <- createWorkbook(),
    sheetName = "data"
)


###### ukládám do sešitu data -------------------------------------------------

writeData(
    wb = data_backup,
    sheet = "data",
    rowNames = FALSE,
    colNames = TRUE,
    x = my_data
)


###### vytvářím dva své styly - jednak tučné písmo, jednak písmo zarovnané
###### doprava v rámci buňky --------------------------------------------------

my_bold_style <- createStyle(textDecoration = "bold")
right_halign_cells <- createStyle(halign = "right")

addStyle(
    wb = data_backup,
    sheet = "data",
    style = my_bold_style,
    rows = rep(1, dim(my_data)[2]),
    cols = c(1:dim(my_data)[2])
)

addStyle(
    wb = data_backup,
    sheet = "data",
    style = right_halign_cells,
    rows = 2:(dim(my_data)[1] + 1),
    cols = 2:dim(my_data)[2],
    gridExpand = TRUE
)


###### nastavuji automatickou šířku sloupce -----------------------------------
 
setColWidths(
    wb = data_backup,
    sheet = "data",
    cols = 1:dim(my_data)[2],
    widths = "auto"
)


###### ukládám workbook -------------------------------------------------------

saveWorkbook(
    wb = data_backup,
    file = "P155b.xlsx",
    overwrite = TRUE
)


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## preprocessing dat ----------------------------------------------------------

#### překódovávám kategorickou proměnnou "Country" ----------------------------

my_data$Country <- factor(
    my_data$Country,
    levels = c("USA", "Japan", "Germany", "Other"),
    labels = c("USA", "Japonsko", "Německo", "Ostatní")
)


## ----------------------------------------------------------------------------

###############################################################################

## Exploratory Data Analysis --------------------------------------------------

#### nejdříve vytvářím diagram závislosti ceny automobilů na jejich
#### koňské síle --------------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "cena_vs_konska_sila.eps",
    width = 8,
    height = 7,
    pointsize = 18
)

par(mar = c(4, 4, 1, 1))


plot(
    Y ~ Horsepower,
    my_data,
    xlab = "koňská síla",
    ylab = "cena"
)

dev.off()


#### --------------------------------------------------------------------------

cairo_ps(
    file = "cena_vs_konska_sila_barevne.eps",
    width = 8,
    height = 7,
    pointsize = 18
)

par(mar = c(4, 4, 1, 1))

plot(
    Y ~ Horsepower,
    my_data,
    xlab = "koňská síla",
    ylab = "cena",
    col = my_data$Country,
    pch = 19
)

legend(
    x = "topleft",
    legend = levels(my_data$Country),
    col = 1:length(my_data$Country),
    pch = 19,
    title = "země původu",
    #cex = 0.85
)

dev.off()


#### --------------------------------------------------------------------------

#### nyní vytvářím matici závislosti cena vs. koňská síla izolovaně pro
#### země původu --------------------------------------------------------------

cairo_ps(
    file = "cena_vs_konska_sila_dle_zeme_puvodu.eps",
    width = 16,
    height = 5.0,
    pointsize = 27
)

par(
    mfrow = c(1, 4),
    mar = c(0, 0, 0, 0),
    oma = c(0.0, 2.0, 0.0, 0.5),
    #tcl = -0.25,
    mgp = c(2, 0.6, 0)
)

for(my_country in levels(my_data$Country)){
    
    par(mar = c(4, 1, 2, 0))
    
    plot(
        Y ~ Horsepower,
        data = my_data[my_data$Country == my_country, ],
        xlab = "koňská síla",
        ylab = "cena",
        main = my_country,
        xlim = c(50, 250),
        ylim = c(0, 45),
        yaxt = "n"
    )
    
    if(which(levels(my_data$Country) == my_country) == 1){        
        axis(2, at = seq(0, 45, 10), labels = seq(0, 45, 10))        
    }
    
    abline(
        lm(
            Y ~ Horsepower,
            data = my_data[my_data$Country == my_country, ]
        ),
        col = "red"
    )
    
    lines(
        seq(
            min(my_data$Horsepower),
            max(my_data$Horsepower),
            length.out = 1000
        ),
        predict(
            lm(
                Y ~ Horsepower,
                data = my_data[my_data$Country == my_country, ]
            ),
            newdata = data.frame("Horsepower" = seq(
                min(my_data$Horsepower),
                max(my_data$Horsepower),
                length.out = 1000
            )),
            interval = "confidence"
        )[, 2],
        lty = "dashed",
        col = "red"
    )
    
    lines(
        seq(
            min(my_data$Horsepower),
            max(my_data$Horsepower),
            length.out = 1000
        ),
        predict(
            lm(
                Y ~ Horsepower,
                data = my_data[my_data$Country == my_country, ]
            ),
            newdata = data.frame("Horsepower" = seq(
                min(my_data$Horsepower),
                max(my_data$Horsepower),
                length.out = 1000
            )),
            interval = "confidence"
        )[, 3],
        lty = "dashed",
        col = "red"
    )
    
}

mtext("cena", side = 2, outer = TRUE, cex = 0.7, line = 1.0)

dev.off()


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## zkoumám aditivitu vs. neaditivitu vztahu mezi cenou vs. koňskou silou
## a zemí původu --------------------------------------------------------------

additive_model <- lm(
    Y ~ Country + Horsepower,
    data = my_data
)

non_additive_model <- lm(
    Y ~ Country + Horsepower + Country : Horsepower,
    data = my_data
)

#### regresní diagnostika -----------------------------------------------------

plot(additive_model)
plot(non_additive_model)


setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_model_name in c("additive_model", "non_additive_model")){
    
    my_model <- get(my_model_name)
    
    setEPS()
    postscript(
        file = paste(
            "residua_vs_vyrovnane_hodnoty_",
            my_model_name,
            ".eps",
            sep = ""
        ),
        width = 8,
        height = 5,
        pointsize = 18
    )

    par(mar = c(4, 4, 1, 1))

    plot(
        resid(my_model) ~ fitted(my_model),
        xlab = expression(
            paste("vyrovnané hodnoty, ", hat(italic(y)), sep = "")
        ),
        ylab = "realizace reziduí",
        xlim = c(0, 42)
    )
    
    dev.off()

}

setwd(mother_working_directory)


#### printable verze odhadů koeficientů obou modelů ---------------------------

print(
    xtable(
        summary(additive_model)$coefficients,
        align = rep("", ncol(summary(additive_model)$coefficients) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)

print(
    xtable(
        summary(non_additive_model)$coefficients,
        align = rep("", ncol(summary(non_additive_model)$coefficients) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)

#### je lepší pouze aditivní model ? ------------------------------------------

anova(non_additive_model)    ## zdá se, že interakční člen je "zbytečný"

anova(
    additive_model,
    non_additive_model
)                            ## dostáváme dle očekávání zcela shodný výsledek
                             ## jako výše -- aditivní model bez interakčního
                             ## členu je v postatě podmínkovou variantou
                             ## složitejšího modelu, resp. jsou si podobné
                             ## a není třeba zamítat nulovou hypotézu
                             ## o dostatečnosti aditivního modelu

print(
    xtable(
        anova(non_additive_model),
        align = rep("", ncol(anova(non_additive_model)) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)


## ----------------------------------------------------------------------------

###############################################################################

## je závislost ceny na zemi významná po očištění o koňskou sílu? -------------

only_country_model <- lm(
    Y ~ Country,
    data = my_data
)

summary(only_country_model)

anova(
    only_country_model
)

anova(
    additive_model
)

anova(
    only_country_model,
    additive_model
)

print(
    xtable(
        anova(only_country_model),
        align = rep("", ncol(anova(only_country_model)) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)

print(
    xtable(
        anova(additive_model),
        align = rep("", ncol(anova(additive_model)) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)


#### tisknu regresní diagnostiku předchozích dvou modelů ----------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "residua_vs_vyrovnane_hodnoty_only_country_model.eps",
    width = 8,
    height = 5,
    pointsize = 18
)

par(mar = c(4, 4, 1, 1))

boxplot(
    list(
        "USA" = resid(
            only_country_model
        )[which(my_data$Country == "USA")],
        "Japonsko" = resid(
            only_country_model
        )[which(my_data$Country == "Japonsko")],
        "Německo" = resid(
            only_country_model
        )[which(my_data$Country == "Německo")],
        "Ostatní" = resid(
            only_country_model
        )[which(my_data$Country == "Ostatní")]
    ),
    xlab = "země",
    ylab = "realizace reziduí"    
)

dev.off()


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





