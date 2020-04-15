library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(openxlsx)
library(gdata)
library(ggplot2)
library(ggsci)
library(DT)
library(tidyverse)
library(ggExtra)
library(cowplot)
options(warn=-1)
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))
#
ui<-renderUI(
  fluidPage(
    title="NAguideR",
    shinyjs::useShinyjs(),
    fluidRow(
      div(
        HTML(
          "<div style='text-align:center;margin-top:5px;margin-right:0px'>
            <a href='#' target=''><img src='imputeRti.png' width='200px'>
            </a>
            </div>"
        )
      )
      #column(6,div(
      #  HTML(
      #    "<div style='text-align:right;margin-top:20px;margin-right:0px'>
      #    <a href='#' target=''><img src='imputeRti.png' width='100px'>
      #    </a>
      #    </div>"
      #  )
      #  )),
      #column(6,div(
      #  HTML(
      #    "<div style='text-align:left;margin-left:-20px'>
      #    <a href='#' target=''><img src='motifeRlogo.png' height='80px'>
      #    </a>
      #    </div>"
      #  )
      #  ))
    ),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js"),
        #tags$style(type="text/css","#methodids{margin-left:2%;margin-right:2%}"),
        tags$style(type="text/css", "
                           #loadmessage {
                     position: fixed;
                     top: 0px;
                     left: 0px;
                     width: 100%;
                     height:100%;
                     padding: 250px 0px 5px 0px;
                     text-align: center;
                     font-weight: bold;
                     font-size: 100px;
                     color: #000000;
                     background-color: #D6D9E4;
                     opacity:0.6;
                     z-index: 105;
                     }
                     "),
        tags$script('
                            var dimension = [0, 0];
                    $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    '),
        tags$style(type="text/css", "
                   #tooltip {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),#F5F5DC
        tags$style(type="text/css", "
                   #tooltip2 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip3 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip4 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltipx5 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   ")
      )
    ),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      id="maintab",
      tabPanel(
        "Welcome",
        uiOutput("welcomeui"),
        icon = icon("home")
      ),
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 1: Upload Original Data",
              tags$span(
                id = 'span1',
                `data-toggle` = "tooltip",
                title = '
                In this part, users can upload their own proteomics expression data and sample information data. The example data can be found when users click "Load example data" below. Detailed descriptions are provided in the "Help" part.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            radioButtons(
              "loaddatatype",
              label = "",
              choices = list("Load experimental data" = 1,"Load example data"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.loaddatatype==1",
              h4("1. Expression data:"),
              radioButtons(
                "fileType_Input",
                label = h5("1.1 File format:"),
                choices = list(".csv/txt" = 1,".xls"=2, ".xlsx" = 3),
                selected = 1,
                inline = TRUE
              ),
              fileInput('file1', h5('1.2 Import your data：'),
                        accept=c('text/csv','text/plain','.xlsx','.xls')),
              checkboxInput('header', 'First row as column names ?', TRUE),
              checkboxInput('firstcol', 'First column as row names ?', FALSE),
              conditionalPanel(condition = "input.fileType_Input==3",
                               numericInput("xlsxindex","Sheet index:",value = 1)),
              conditionalPanel(condition = "input.fileType_Input==2",
                               numericInput("xlsxindex","Sheet index:",value = 1)),
              conditionalPanel(condition = "input.fileType_Input==1",
                               radioButtons('sep', h5('Separator：'),
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t',
                                              BlankSpace=' '),
                                            ',')),
              tags$hr(style="border-color: #B2B2B2;"),
              h4("2. Samples information data:"),
              radioButtons(
                "mchuanshaodxyibanfileType_Input_fenzu",
                label = h5("2.1 File format:"),
                choices = list(".csv/txt" = 1,".xls"=2, ".xlsx" = 3),
                selected = 1,
                inline = TRUE
              ),
              fileInput('mchuanshaodxyibanfile1_fenzu', h5('2.2 Import your data：'),
                        accept=c('text/csv','text/plain','.xlsx','.xls')),
              checkboxInput('mchuanshaodxyibanheader_fenzu', 'First row as column names ?', TRUE),
              checkboxInput('mchuanshaodxyibanfirstcol_fenzu', 'First column as row names ?', FALSE),
              conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==3",
                               numericInput("mchuanshaodxyibanxlsxindex_fenzu","Sheet index:",value = 1)),
              conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==2",
                               numericInput("mchuanshaodxyibanxlsxindex_fenzu","Sheet index:",value = 1)),
              conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==1",
                               radioButtons('mchuanshaodxyibansep_fenzu', h5('Separator：'),
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t',
                                              BlankSpace=' '),
                                            ','))
            ),
            conditionalPanel(
              condition = "input.loaddatatype==2",
              downloadButton("loaddatadownload1","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED"),
              tags$hr(style="border-color: grey;"),
              downloadButton("loaddatadownload2","Download example sample group data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
            )
          ),
          mainPanel(
            width = 9,
            hr(),
            h4("1. Expression data："),
            selectInput("datatypex","The first few column types:",choices = c("Peptides+Charges+Proteins"=3,"Peptides+Charges"=1,"Peptides+Proteins"=2,"Proteins"=4,"Others"=5),selected = 3),
            hr(),
            dataTableOutput("peaksdata"),
            h4("2. Samples information data："),
            dataTableOutput("samplesdata")
          )
        ),
        icon = icon("upload")
      ),
      tabPanel(
        "NA Overview",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 2: NA Overview",
              tags$span(
                id = 'span2',
                `data-toggle` = "tooltip2",
                title = '
                NA here means missing value. In this part, These proteins/peptides with excessively high proportion of NA and large coefficient of variation (CV) will be removed.
                You can click "Input data check" part and check your input data quality. Therefore, this step is also called "Data quality control".
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            textInput("natype",h5("1. Missing value type:"),value = "NA"),
            bsTooltip("natype",'The type of missing value in the expression data.',
                      placement = "right",options = list(container = "body")),
            #textInput("natypecol",h5("2. Colors for plot by row:"),value = "#A11315;#8C86FF"),
            hr(),
            checkboxInput('fenzukaolvif', '2. Count NA by each group or not?', TRUE),
            bsTooltip("fenzukaolvif",'If true, NAguideR will count the NA number in every group, otherwise, it will count the NA number across all groups.',
                      placement = "right",options = list(container = "body")),
            #conditionalPanel(
            #  condition = 'input.fenzukaolvif==true',
            #  checkboxInput("keepzeroif","2.1. Keep NA as zero (0) or not?",FALSE),
            #  bsTooltip("keepzeroif",'If true, that means when all of the values of one protein/peptide are NA in one group, NAguideR will keep these NA as 0.',
            #            placement = "right",options = list(container = "body"))
            #),
            numericInput('naratio', h5('3. NA ratio:'), 0.5,max = 1,min = 0,step = 0.1),
            bsTooltip("naratio",'The threshold of NA ratio. One protein/peptide with NA ratio above this threshold will be removed.',
                      placement = "right",options = list(container = "body")),
            checkboxInput('mediannormif', '4. Median normalization or not?', TRUE),
            bsTooltip("mediannormif",'If true, the values in expression matrix will be devided by its column median value to make the samples to have the same median. (Please note, NAguideR was not designed to perform sophisticated normalization analysis. Any normalized datasets with NA can be accepted for analysis).',
                      placement = "right",options = list(container = "body")),
            checkboxInput('logif', '5. Log or not?', TRUE),
            bsTooltip("logif",'If true, the values in expression matrix will be log-transformed with base 2.',
                      placement = "right",options = list(container = "body")),
            numericInput('cvyuzhi', h5('6. CV threshold (raw scale):'), 0.3,max = 1,min = 0,step = 0.1),
            bsTooltip("cvyuzhi",'The threshold of coefficient of variation (CV). One protein/peptide with CV above this threshold will be removed. "raw scale" here means the values without log-transformation are used to calculate the CV.',
                      placement = "right",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            numericInput("preheight",h5("Height for figure:"),value = 900)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id="subnav",
              tabPanel(
                "NA Distribution",
                #hr(),
                div(
                  id="nafenbuid",
                  radioButtons(
                    "nafenbu",
                    label = h4(""),
                    choices = list("NA data"=1,"Plot by column"=2,"Plot by row"=3),
                    selected = 1,
                    inline = TRUE
                  )
                ),
                bsTooltip("nafenbuid",'1. NA data: the result that NAguideR recognize the missing values in the expression matrix; 2. Plot by column: the figure shows the distribution of missing values in each sample; 3. Plot by row: the figure show the distribution of missing values in every protein/peptide.',
                          placement = "left",options = list(container = "body")),
                actionButton("mcsbtn_nafenbu","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #CD853F; border-color: #CD853F"),
                tags$hr(style="border-color: grey;"),
                hidden(
                  div(
                    id="mcsbtn_nafenbu_hid",
                    conditionalPanel(
                      condition = "input.nafenbu==1",
                      downloadButton("nadatadfdl","Download"),
                      dataTableOutput("nadatadf")
                    ),
                    conditionalPanel(
                      condition = "input.nafenbu==2",
                      downloadButton("naplotbycolumndl","Download"),
                      plotOutput("naplotbycolumn")
                    ),
                    conditionalPanel(
                      condition = "input.nafenbu==3",
                      downloadButton("naplotbyrowdl","Download"),
                      plotOutput("naplotbyrow")
                    )
                  )
                )
              ),
              tabPanel(
                "NA Filter",
                hr(),
                #radioButtons(
                #  "nafilterradiobutton",
                #  label = h4(""),
                #  choices = list("Filtered data"=1,"Data without NA"=2,"Data with NA"=3),
                #  selected = 1,
                #  inline = TRUE
                #),
                actionButton("mcsbtn_nafilter","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #CD853F; border-color: #CD853F"),
                tags$hr(style="border-color: grey;"),
                hidden(
                  div(
                    id="mcsbtn_nafilter_hid",
                    downloadButton("filtereddatadfdl","Download"),
                    dataTableOutput("filtereddatadf")
                  )
                )
                #conditionalPanel(
                #  condition = "input.nafilterradiobutton==1",
                #  downloadButton("filtereddatadfdl","Download"),
                #  dataTableOutput("filtereddatadf")
                #),
                #conditionalPanel(
                #  condition = "input.nafilterradiobutton==2",
                #  downloadButton("datanonadl","Download"),
                #  dataTableOutput("datanona")
                #),
                #conditionalPanel(
                #  condition = "input.nafilterradiobutton==3",
                #  downloadButton("datahasnadl","Download"),
                #  dataTableOutput("datahasna")
                #)
              ),
              tabPanel(
                "Input data check",
                hr(),
                uiOutput("inputdatacheck1"),
                uiOutput("inputdatacheck2")
              )
            )
          )
        ),
        icon = icon("binoculars")
      ),
      tabPanel(
        "Methods",
        hr(),
        div(
          style="text-align:center;margin-left:200px;margin-right:200px",
          h3(
            "Step 3: Missing value imputation. All methods have been classified based on their algorithm, please select the imputation methods you want (by default, fast methods are chosen in each category), then click the 'Calculate' button.",
            tags$span(
              id = 'span3',
              `data-toggle` = "tooltip3",
              title = '
                In this part, users should select the imputation methods first. With regard to the running time, we set these fast methods chosen by default. Detailed instruction of each methods can be found in the references below or in the "Help" part.
                ',
              tags$span(class = "glyphicon glyphicon-question-sign")
            )
          ),
          actionButton("mcsbtn_imputationII","Calculate",icon("paper-plane"),width ='200px',
                       style="color: #fff; background-color: #CD853F; border-color: #CD853F")
        ),
        hr(),
        mainPanel(
          id="methodids",
          width = 12,
          fixedRow(
            column(
              width = 4,
              panel(
                "",
                heading = "A. Single value approaches",
                status = "info",
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("zeromethodif", "Using zero method or not?",TRUE),
                      heading = "Method 1: Zero",
                      status = "success",
                      footer = a(href="https://doi.org/10.1021/acs.jproteome.5b00981",h6("DOI: 10.1021/acs.jproteome.5b00981"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("minimummethodif", "Using minimum method or not?",TRUE),
                      heading = "Method 2: Minimum",
                      status = "success",
                      footer = a(href="https://doi.org/10.1038/s41586-019-0987-8",h6("DOI: 10.1038/s41586-019-0987-8"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("colmedianif", "Using colmedian method or not?",TRUE),
                      heading = "Method 3: Column median (colmedian)",
                      status = "success",
                      footer = a(href="https://CRAN.R-project.org/package=e1071",h6("Package: e1071"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("rowmedianif", "Using row median method or not?",TRUE),
                      heading = "Method 4: Row median (rowmedian)",
                      status = "success",
                      footer = a(href="https://CRAN.R-project.org/package=e1071",h6("Package: e1071"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("mindetif", "Using mindet method or not?",TRUE),
                      heading = "Method 5: Deterministic minimal value (mindet)",
                      status = "success",
                      footer = a(href="https://CRAN.R-project.org/package=imputeLCMD",h6("Package: imputeLCMD"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("minprobif", "Using minprob method or not?",TRUE),
                      heading = "Method 6: Stochastic minimal value (minprob)",
                      status = "success",
                      footer = a(href="https://CRAN.R-project.org/package=imputeLCMD",h6("Package: imputeLCMD"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("PIif", "Using perseus imputation method or not?",TRUE),
                      numericInput("piwidth",h5("Width:"),value = 0.3),
                      numericInput("pidownshift",h5("Down shift:"),value = 1.8),
                      heading = "Method 7: Perseus imputation (PI)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1038/nmeth.3901",h6("DOI:10.1038/nMeth.3901"),target="_blank")
                    )
                  )
                )
              )
            ),
            column(
              width = 4,
              panel(
                "",
                heading = "B. Global structure approaches",
                status = "warning",
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("svdmethodif", "Using svd method or not?",TRUE),
                      heading = "Method 8: Singular value decomposition (svd)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1093/bioinformatics/17.6.520",h6("DOI: 10.1093/bioinformatics/17.6.520"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("mleif", "Using mle method or not?",TRUE),
                      heading = "Method 9: Maximum likelihood estimation (mle)",
                      status = "success",
                      footer = a(href="https://CRAN.R-project.org/package=norm",h6("Package: norm"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("impseqif", "Using impseq method or not?",TRUE),
                      heading = "Method 10: Sequential imputation (impseq)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1016/j.compbiolchem.2007.07.001",h6("DOI: 10.1016/j.compbiolchem.2007.07.001"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("impseqrobif", "Using impseqrob method or not?",TRUE),
                      heading = "Method 11: Robust sequential imputation (impseqrob)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1016/j.compbiolchem.2008.07.019",h6("DOI: 10.1016/j.compbiolchem.2008.07.019"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("bpcaif", "Using bpca method or not?",FALSE),
                      heading = "Method 12: Bayesian principal component analysis (bpca)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1093/bioinformatics/btg287",h6("DOI: 10.1093/bioinformatics/btg287"),target="_blank")
                    )
                  )
                )
              )
            ),
            column(
              width = 4,
              panel(
                "",
                heading = "C. Local similarity approaches",
                status = "danger",
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("KNNmethodif", "Using knn method or not?",TRUE),
                      heading = "Method 13: K-nearest neighbor (knn)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1093/bioinformatics/17.6.520",h6("DOI: 10.1093/bioinformatics/17.6.520"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("seqknnif", "Using seq-knn method or not?",TRUE),
                      heading = "Method 14: Sequential knn (seq-knn)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1186/1471-2105-5-160",h6("DOI: 10.1186/1471-2105-5-160"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("QRILCif", "Using qr method or not?",TRUE),
                      heading = "Method 15: Quantile regression (qr)",
                      status = "success",
                      footer = a(href="https://CRAN.R-project.org/package=imputeLCMD",h6("Package: imputeLCMD"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("llsif", "Using lls method or not?",TRUE),
                      heading = "Method 16: Local least squares (lls)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1093/bioinformatics/bth499",h6("DOI: 10.1093/bioinformatics/bth499"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("GRRif", "Using GRR method or not?",TRUE),
                      heading = "Method 17: Glmnet Ridge Regression (GRR)",
                      status = "success",
                      footer = a(href="https://github.com/WangLab-MSSM/DreamAI",h6("Package: DreamAI"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("micenormif", "Using mice-norm method or not?",TRUE),
                      heading = "Method 18: Multiple imputation bayesian linear regression (mice-norm)",
                      status = "success",
                      footer = a(href="https://doi.org/10.18637/jss.v045.i03",h6("DOI: 10.18637/jss.v045.i03"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("trknnif", "Using trknn method or not?",FALSE),
                      heading = "Method 19: Truncation knn (trknn)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1186/s12859-017-1547-6",h6("DOI: 10.1186/s12859-017-1547-6"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("irmif", "Using irm method or not?",FALSE),
                      heading = "Method 20: Iterative robust model (irm)",
                      status = "success",
                      footer = a(href="https://doi.org/10.18637/jss.v074.i07",h6("DOI: 10.18637/jss.v074.i07"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("GMSif", "Using GMS method or not?",FALSE),
                      heading = "Method 21: Generalized Mass Spectrum (GMS)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1093/bioinformatics/btz488",h6("DOI: 10.1093/bioinformatics/btz488"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("micecartif", "Using mice-cart method or not?",FALSE),
                      heading = "Method 22: Multiple imputation classification and regression trees (mice-cart)",
                      status = "success",
                      footer = a(href="https://doi.org/10.18637/jss.v045.i03",h6("DOI: 10.18637/jss.v045.i03"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("RFif", "Using rf method or not?",FALSE),
                      numericInput("rfntrees",h5("Number of trees:"),value = 20),
                      heading = "Method 23: Random forest model (rf)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1093/bioinformatics/btr597",h6("DOI: 10.1093/bioinformatics/btr597"),target="_blank")
                    )
                  )
                )
              )
            )
          )
        ),
        icon = icon("cogs")
      ),
      tabPanel(
        "Results and Assessments",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 4: Results and Assessments",
              tags$span(
                id = 'span4',
                `data-toggle` = "tooltip4",
                title = '
                Performance evaluation. In this part, users can obtain (1) the whole imputed results, (2) the scores under every classic criterion, and (3) the scores under every proteomic criterion. Detailed descriptions are provided in the "Help" part.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            h4("1. Parameters for 'Results'"),
            h5(
              "1.1. Select one method: ",
              tags$span(
                id = 'spanx5',
                `data-toggle` = "tooltipx5",
                title = '
                The Data filtered in Step 2 are derived individually with each imputation method chosen by users in Step 3. Therefore, users can check every imputed result by selecting relative method here.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            uiOutput(
              "imputaionIIresui"
            ),
            hr(),
            h4("2. Parameters for 'Criteria'"),
            checkboxInput("customclassiccriteriaif","2.1. Customize the classic criteria or not?",FALSE),
            bsTooltip("customclassiccriteriaif",'Here users can customize the classic criteria and set the weighting of different preset criteria for specific experimental design and aims. By default, all classic criteria are selected with same weightings.',
                      placement = "right",options = list(container = "body")),
            conditionalPanel(
              condition = "input.customclassiccriteriaif==true",
              selectInput("classiccriteriaxuabze",h5("2.1.1. Please select the criterion/criteria you want:"),choices = c("NRMSE","SOR","ACC_OI","PSS"),selected = c("NRMSE","SOR","ACC_OI","PSS"),multiple = TRUE),
              textInput("classiccriteriaweight",h5("2.1.2. Please set the weighting for each criterion you select:"),value = "1;1;1;1"),
              bsTooltip("classiccriteriaweight",'Please note, a. the numbers of the preset criteria and weights should be equal, which means if you select three criteria, you should type in three weightings; b. the weightings should be separated by semicolons.',
                        placement = "right",options = list(container = "body"))
            ),
            checkboxInput("customproteomiccriteriaif","2.2. Customize the proteomic criteria or not?",FALSE),
            bsTooltip("customproteomiccriteriaif",'Here users can customize the proteomic criteria and set the weighting of different preset criteria for specific experimental design and aims. By default, all proteomic criteria are selected with same weightings.',
                      placement = "right",options = list(container = "body")),
            conditionalPanel(
              condition = "input.customproteomiccriteriaif==true",
              selectInput("proteomiccriteriaxuabze",h5("2.2.1. Please select the criterion/criteria you want:"),choices = c("Charge","PepProt","CORUM","PPI"),selected = c("Charge","PepProt","CORUM","PPI"),multiple = TRUE),
              textInput("proteomiccriteriaweight",h5("2.2.2. Please set the weighting for each criterion you select:"),value = "1;1;1;1"),
              bsTooltip("proteomiccriteriaweight",'Please note, a. the numbers of the preset criteria and weights should be equal, which means if you select three criteria, you should type in three weightings; b. the weightings should be separated by semicolons.',
                        placement = "right",options = list(container = "body"))
            ),
            tags$hr(style="border-color: grey;"),
            numericInput("pinjiafigheight",h5("Figure height:"),value = 800)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id="subnav",
              tabPanel(
                "Results",
                hr(),
                downloadButton("imputaionIIdatadl","Download"),
                dataTableOutput("imputaionIIdata")
              ),
              tabPanel(
                "Classic criteria",
                hr(),
                actionButton("mcsbtn_imputation","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #CD853F; border-color: #CD853F"),
                tags$hr(style="border-color: grey;"),
                hidden(
                  div(
                    id="mcsbtn_imputation_hid",
                    h4("1. Comprehensive ranks under classic criteria:"),
                    downloadButton("assessrankdl","Download"),
                    dataTableOutput("assessrank"),
                    hr(),
                    column(
                      width = 6,
                      h4("2. Normalized root mean squared Error (NRMSE):"),
                      downloadButton("nrmseresdl","Download"),
                      dataTableOutput("nrmseres")
                    ),
                    column(
                      width = 6,
                      h4("3. NRMSE-based sum of ranks (SOR):"),
                      downloadButton("sornrmseresdl","Download"),
                      dataTableOutput("sornrmseres")
                    ),
                    column(
                      width = 6,
                      h4("4. Procrustes sum of squared errors (PSS):"),
                      downloadButton("pssresdl","Download"),
                      dataTableOutput("pssres")
                    ),
                    column(
                      width = 6,
                      h4("5. Average correlation coefficient between original value and imputed value (ACC_OI):"),
                      downloadButton("avgcordl","Download"),
                      dataTableOutput("avgcor")
                    ),
                    hr(),
                    h4("6. Figures:"),
                    downloadButton("assessplotdl","Download"),
                    plotOutput("assessplot")
                  )
                )
              ),
              tabPanel(
                "Proteomic criteria",
                hr(),
                actionButton("mcsbtn_imputationpro","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #CD853F; border-color: #CD853F"),
                tags$hr(style="border-color: grey;"),
                hidden(
                  div(
                    id="mcsbtn_imputationpro_hid",
                    conditionalPanel(
                      condition = "input.datatypex==1",
                      h4("1. Average correlation coefficient between peptides with different charges (ACC_Charge):"),
                      downloadButton("accpcresdl1","Download"),
                      dataTableOutput("accpcres1"),
                      h4("2. Histogram of ACC_Charge:"),
                      downloadButton("histaccpcplotdl1","Download"),
                      plotOutput("histaccpcplot1")
                    ),
                    conditionalPanel(
                      condition = "input.datatypex==2",
                      h4("1. Comprehensive ranks under proteomic criteria:"),
                      downloadButton("accpprankdl2","Download"),
                      dataTableOutput("accpprank2"),
                      column(
                        width = 4,
                        h4("2. Average correlation coefficient between peptides in a same protein (ACC_PepProt):"),
                        downloadButton("accppresdl2","Download"),
                        dataTableOutput("accppres2")
                      ),
                      column(
                        width = 4,
                        h4("3. Average correlation coefficient between protein complexes (ACC_CORUM):"),
                        downloadButton("acccomplexresdl2","Download"),
                        dataTableOutput("acccomplexres2")
                      ),
                      column(
                        width = 4,
                        h4("4. Average correlation coefficient between protein complexes (ACC_PPI):"),
                        downloadButton("acchumapresdl2","Download"),
                        dataTableOutput("acchumapres2")
                      ),
                      hr(),
                      column(
                        width = 4,
                        h4("5. Histogram of ACC_PepProt:"),
                        downloadButton("histaccppplotdl2","Download"),
                        plotOutput("histaccppplot2")
                      ),
                      column(
                        width = 4,
                        h4("6. Histogram of ACC_CORUM:"),
                        downloadButton("histacccomplexplotdl2","Download"),
                        plotOutput("histacccomplexplot2")
                      ),
                      column(
                        width = 4,
                        h4("7. Histogram of ACC_PPI:"),
                        downloadButton("histacchumapplotdl2","Download"),
                        plotOutput("histacchumapplot2")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.datatypex==3",
                      h4("1. Comprehensive ranks under proteomic criteria:"),
                      downloadButton("accpprankdl3","Download"),
                      dataTableOutput("accpprank3"),
                      column(
                        width = 6,
                        h4("2. Average correlation coefficient between peptides with different charges (ACC_Charge):"),
                        downloadButton("accpcresdl3","Download"),
                        dataTableOutput("accpcres3")
                      ),
                      column(
                        width = 6,
                        h4("3. Average correlation coefficient between peptides in a same protein (ACC_PepProt):"),
                        downloadButton("accppresdl3","Download"),
                        dataTableOutput("accppres3")
                      ),
                      column(
                        width = 6,
                        h4("4. Average correlation coefficient between protein complexes (ACC_CORUM):"),
                        downloadButton("acccomplexresdl3","Download"),
                        dataTableOutput("acccomplexres3")
                      ),
                      column(
                        width = 6,
                        h4("5. Average correlation coefficient between protein complexes (ACC_PPI):"),
                        downloadButton("acchumapresdl3","Download"),
                        dataTableOutput("acchumapres3")
                      ),
                      hr(),
                      h4("6. Figures:"),
                      downloadButton("accproallplotdl","Download"),
                      plotOutput("accproallplot")
                    ),
                    conditionalPanel(
                      condition = "input.datatypex==4",
                      h4("1. Comprehensive ranks under proteomic criteria:"),
                      downloadButton("accpprankdl4","Download"),
                      dataTableOutput("accpprank4"),
                      column(
                        width = 6,
                        h4("2. Average correlation coefficient between protein complexes (ACC_CORUM):"),
                        downloadButton("acccomplexresdl4","Download"),
                        dataTableOutput("acccomplexres4")
                      ),
                      column(
                        width = 6,
                        h4("3. Average correlation coefficient between protein complexes (ACC_PPI):"),
                        downloadButton("acchumapresdl4","Download"),
                        dataTableOutput("acchumapres4")
                      ),
                      column(
                        width = 6,
                        h4("4. Histogram of ACC_CORUM:"),
                        downloadButton("histacccomplexplotdl4","Download"),
                        plotOutput("histacccomplexplot4")
                      ),
                      column(
                        width = 6,
                        h4("5. Histogram of ACC_PPI:"),
                        downloadButton("histacchumapplotdl4","Download"),
                        plotOutput("histacchumapplot4")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.datatypex==5",
                      h3("There are no assessment here!")
                    )
                  )
                )
              ),
              tabPanel(
                "Final check",
                hr(),
                uiOutput("finalcheck1"),
                uiOutput("finalcheck2")
              ),
              tabPanel(
                "Targeted check (Optional)",
                div(
                  style="text-align:left;margin-top:15px;margin-left:10px;margin-right:300px;font-size:90%;",
                  HTML("Please note: This function is optinal and designed for many biologists with specific experimental aims, for example, some users may want to check a particular peptide/protein
                       (i.e. spiked-in standard peptides, proteins, or known housekeeping proteins like beta-actin, etc.) before and after imputation.")
                ),
                hr(),
                div(
                  style="text-align:left;margin-left:150px;",
                  textInput("targettext",h5("Please type in a protein id or peptide sequance as that in the original expression data:"),value = "", width='450px',placeholder = "such as P29692 or ESVPEFPLS[Phospho (STY)]PPK")
                ),
                actionButton("mcsbtn_targettext","Check",icon("paper-plane"), width='150px',
                             style="color: #fff; background-color: #CD853F; border-color: #CD853F;margin-left:300px;"),
                hr(),
                hidden(
                  div(
                    id="mcsbtn_targettext_hid",
                    downloadButton("targettextplotdl","Download"),
                    hr(),
                    plotOutput("targettextplot")
                  )
                )
              )
            )
          )
        ),
        icon = icon("table")
      ),
      tabPanel(
        "Help",
        navlistPanel(
          id="helpmanualid",
          "Detailed description",
          tabPanel(
            "1. Overview of NAguideR",
            div(style="text-align:left;margin-top:15px;font-size:140%;",HTML("<b>1.1 Abstract</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px",
              "Mass-spectrometry (MS) based quantitative proteomics experiments frequently generate data with missing values, which may profoundly affect downstream analyses.
              A wide variety of missing value imputation methods have been established to deal with the missing-value issue. To date,
              however, there is a scarcity of efficient, systematic, and easy-to-handle tools that are tailored for proteomics community.
              Herein, we developed a user-friendly and powerful web tool, NAguideR, to enable implementation and evaluation of different
              missing value methods offered by twenty popular missing-value imputation algorithms. Evaluation of data imputation results
              can be performed through classic computational criteria and, unprecedentedly, proteomic empirical criteria such as
              quantitative consistency between different charge-states of the same peptide, different peptides belonging to the same
              proteins, and individual proteins participating functional protein complexes. We applied NAguideR into three label-free
              proteomic datasets featuring peptide-level, protein-level, and phosphoproteomic variables respectively, all generated by
              data independent mass spectrometry (DIA-MS) with substantial biological replicates. The results indicate that NAguideR is
              able to discriminate the optimal imputation methods that are facilitating DIA-MS experiments over those sub-optimal and
              low-performance algorithms. NAguideR web-tool further provides downloadable tables and figures supporting flexible data
              analysis and interpretation. The flowchart below summarizes the process of data analysis in NAguideR."
            ),
            div(style="text-align:center;margin-top: 8px",
                a(href='#',
                  img(src='Figure1app.png',height=1200))),
            div(style="text-align:left;margin-top:20px;font-size:140%;",HTML("<b>1.2 What NAguideR exactly does in each step ?</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "As described above, there are four main steps in the data analysis process of NAguideR: (1) Upload of proteomics data; (2) Data quality control;
              (3) Missing value imputation; (4) Performance evaluation. However, many users care about the detailed operation in each step.
              The figure below shows the major steps of the data analysis process in NAguideR. We take two groups of samples
              (five biological replicates in each group, labeled A1, A2, A3, A4, A5, B1, B2, B3, B4, B5 in the original intensity data)
              for example. Feature means the identified proteins/peptides."
            ),
            div(style="text-align:center;margin-top:8px;margin-bottom:30px;",
                a(href='#',
                  img(src='FigureS2app.png',height=1000))),
            icon = icon("dashboard")
          ),
          tabPanel(
            "2. User manual",
            div(style="text-align:left;margin-top:15px;font-size:140%;",HTML("<b>2.1 Input data preparation</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              HTML("NAguideR supports four basic file formats (.csv, .txt, .xlsx, .xls). Before analysis, users should prepare two required data: (1) Proteomics expression data and (2) Sample information data.
              The data required here could be readily generated based on results of several popular tools such as <a href='https://www.maxquant.org/' target='_blank'>MaxQuant</a>,
              <a href='http://www.bioinfor.com/peaks-studio/' target='_blank'>PEAKS</a>, <a href='https://biognosys.com/shop/spectronaut' target='_blank'>Spectronaut</a>, and so on. Then
              can upload the two data into NAguideR with right formats respectively and start subsequent analysis.")
            ),
            div(style="text-align:left;margin-top:10px;font-size:130%;",HTML("<b>2.1.1 Proteomics expression data</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "There are four types of proteomics expression data supported in NAguideR ('Peptides+Charges+Proteins', 'Peptides+Charges', 'Peptides+Proteins', 'Proteins'), among which the main differences are the first few columns.
              In addition, users may upload other kinds of omics data (i.e. Genomics, Metabolomics), they can choose the fifth type ('Others'), please note, the fifth type can not generate the results based on those protomic criteria."
            ),
            div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.1.1 Expression data with peptide sequences, peptide charge states, and protein ids</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              HTML("In this situation, peptide sequences, peptide charge states, and protein ids are sequentially provided in the first three columns of input file. Peptide sequences in
              the first column can be peptides with any post-translational modification (PTM, written in any routine format) or stripped peptides (without PTM). The second column is peptide charge states. The protein ids
              in the third column should be <a href='https://www.uniprot.org/' target='_blank'>UniProt</a> ids. From the fourth column, peptides/proteins expression intensity or signal abundance in every sample should be listed.
              The data structure is shown as below:")
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='datapreparepng_Page1.png',height=700))),
            div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.1.2 Expression data with peptide sequences and peptide charge states</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              HTML("Similar to the above situation, peptide sequences and peptide charge states are sequentially provided in the first two columns of input file. Peptide sequences in
              the first column can be peptides with post-translational modification (PTM) or stripped peptides (without PTM). The second column is peptide charge states.
              From the third column, peptides/proteins expression intensity or signal abundance in every sample should be listed. The data structure is shown as below:")
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='datapreparepng_Page2.png',height=700))),
            div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.1.3 Expression data with peptide sequences, and protein ids</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              HTML("Under this circumstance, peptide sequences, and protein ids are sequentially provided in the first two columns of input file. Peptide sequences in
              the first column can be peptides with post-translational modification (PTM) or stripped peptides (without PTM). The protein ids
              in the second column should be <a href='https://www.uniprot.org/' target='_blank'>UniProt</a> ids. From the third column, peptides/proteins expression intensity or signal abundance in every sample should be listed. The data structure is shown as below:")
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='datapreparepng_Page3.png',height=700))),
            div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.1.4 Expression data with protein ids</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              HTML("In this situation, protein ids are provided in the first columns of input file. The protein ids here
              should be <a href='https://www.uniprot.org/' target='_blank'>UniProt</a> ids. From the second column,peptides/proteins expression intensity or signal abundance in every sample should be listed. The data structure is shown as below:")
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='datapreparepng_Page4.png',height=700))),
            div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.1.5 Other kinds of omics data</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              HTML("If users want to use NAguideR for other omics data (i.e. genomics, metabolomics), gene/metabolite ids/names should be provided in the first columns of input file.
              From the second column, genes/metabolites expression intensity or signal abundance in every sample should be listed. The data structure may be shown as below:")
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='datapreparepng_Page6.png',height=700))),
            div(style="text-align:left;margin-top:10px;font-size:130%;",HTML("<b>2.1.2 Sample information data</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "Sample information here means that users should provide sample group identity information. This information could e.g., enable filtration strategy for different group respectively in the quality control step. The sample names are in the first column and their orders are same as those in the expression data. Group information is in the second column. The data structure is shown as below:"
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='datapreparepng_Page5.png',height=700))),
            div(style="text-align:left;margin-top:16px;font-size:140%;",HTML("<b>2.2 Operating Procedure of NAguideR (Four steps)</b>")),
            div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 1. Uploading proteomics expression data</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "When preparing required data, users can click 'Import data' and upload their own data in the left panel:"
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='importdata.png',width=1200))),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "If users want to check the example data first, they can choose 'Load example data' and download these example data by clicking relative button:"
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='importdata2.png',width=1200))),
            div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 2. Data quality control</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "After uploading the right data, users can click 'NA Overview'. In this part, users can check the NA distribution in their data ('NA distribution' part) and
              those proteins/peptides with excessively high proportion of NA and large coefficient of variation (CV) will be removed ('Filter' part). After setting suitable
              parameters, just click the 'Calculate' button."
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='Qualitycontrol.png',width=1200))),
            div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 3. Missing value imputation</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "After data quality control, users can click 'Methods'. In this step, users should select the imputation methods first. With regard to the running time, we set these fast methods (left part, 15 methods) chosen by default. If users choose those slow methods (right part, 5 methods), that means the running time will be longer."
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='missingvalueimpute.png',width=1200))),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "After selecting suitable methods, users need to click 'Calculate' button, and a popup window will be jumped out to show the selected methods, then click 'OK' button and continue:"
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='missingvalueimpute2.png',width=1200))),
            div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 4. Performance evaluation</b>")),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "Click 'Results and Assessments'. In this step, based on the methods chosen above, the data with NA will be imputed and shown in the 'Results' panel, then the results will be evaluated
              under the four classic criteria and the four proteomic criteria, shown as below:"
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='results1.png',width=1200))),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "The tables and figures are provided here under the four classic criteria. 1. This table shows the comprehensive ranks of every imputation method; 2-5, the tables show the scores of every imputation method based on 'Normalized root mean squared Error (NRMSE)',
              'NRMSE-based sum of ranks (SOR)', 'Procrustes sum of squared errors (PSS)', and 'Average correlation coefficient between original value and imputed value (ACC_OI)', respectively; 6. Figures here show the normalized scores of every imputation method
              under the four classic criteria. 'Normalized Values' here means every score divides by corresponding max value."
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='criteriaresultspng_Page1.png',width=1100))),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
              "The tables and figures are provided here under the four proteomic criteria. 1. This table shows the comprehensive ranks of every imputation method; 2-5, the tables show the scores of every imputation method based on
              'Average correlation coefficient between peptides with different charges (ACC_Charge)', 'Average correlation coefficient between peptides in a same protein (ACC_PepProt)', 'Average correlation coefficient between protein complexes (ACC_CORUM)',
              'Average correlation coefficient between protein complexes (ACC_PPI)', respectively; 6. Figures here show the correlation coefficient distribution of the original values and the imputed values from every imputation method
              under the four proteomic criteria."
            ),
            div(style="text-align:center;margin-top:8px;",
                a(href='#',
                  img(src='criteriaresultspng_Page2.png',width=1100))),
            div(
              style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;margin-bottom:30px;",
              HTML("If you have any questions, comments or suggestions about NAguideR, please feel free to contact: <u>wsslearning@omicsolution.com</u>. We really appreciate that you use NAguideR, and your suggestions should be valuable to its improvement in the future.")
            ),
            icon = icon("file-alt")
          ),
          widths=c(3,9)
        ),
        icon = icon("info-circle")
      )
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #ui
  output$welcomeui<-renderUI({
    screenwidth<-input$dimension[1]
    if(is.null(screenwidth)){
      return(NULL)
    }else{
      if(screenwidth<=1024){
        imgwidth<-400
      }
      else if(screenwidth>1024 & screenwidth<=1440){
        imgwidth<-500
      }
      else{
        imgwidth<-600
      }
    }

    fluidRow(
      #div(style="text-align:center",h1("~~Welcome~~")),
      div(
        id="mainbody",
        column(3),
        column(
          6,
          div(style="text-align:left;margin-top:20px;font-size:140%;color:darkred",
              HTML("~~ <em>Dear Users, Welcome to NAguideR</em> ~~")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px",
              HTML("<b>NAguideR</b> is a web-based tool, which integrates 23 commonly used missing value imputation methods and provides two categories of evaluation criteria (4 classic criteria and 4 proteomic criteria) to assess the imputation performance of various methods. We hope this tool could help scientists impute the missing values systematically and present valuable guidance to select one proper method for their own data. In addition, this tool supports both online access and local installation.")),
          div(style="text-align:center;margin-top: 50px",
              a(href='#',
                img(src='NAguideR_home_small.jpg',height=imgwidth))),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;margin-top:20px;font-size:120%",
              HTML("Basically, there are four main steps in NAguideR:<br />"),
              HTML("&nbsp;&nbsp;1. Uploading proteomics expression data and sample information data;<br />"),
              HTML("&nbsp;&nbsp;2. Data quality control;<br />"),
              HTML("&nbsp;&nbsp;3. Missing value imputation;<br />"),
              HTML("&nbsp;&nbsp;4. Performance evaluation;<br />"),
              HTML("After this, NAguideR can provide valuable guidance for users to select one proper method for their own data based on the evaluation results. Detailed introduction can be found in the <em><b>Help</b></em> part.<br />"),
              HTML("<br />"),
              HTML("Finally, NAguideR is developed by <a href='https://shiny.rstudio.com/' target='_blank'>R shiny (Version 1.3.2)</a>, and is free and open to all users with no login requirement. It can be readily accessed by all popular web browsers including Google Chrome, Mozilla Firefox, Safari and Internet Explorer 10 (or later), and so on. We would highly appreciate that if you could send your feedback about any bug or feature request to Shisheng Wang at <u>wsslearning@omicsolution.com</u>.")),
          div(style="text-align:center;margin-top:20px;font-size:140%;color:darkgreen",
              HTML("<br />"),
              HTML("^_^ <em>Enjoy yourself in NAguideR</em> ^_^")),
          tags$hr(style="border-color: grey60;"),
          div(style="text-align:center;margin-top: 20px;font-size:100%",
              HTML(" &copy; 2019 <a href='https://www.yslproteomics.org/' target='_blank'>Yansheng Liu's Group</a> and <a href='http://english.cd120.com/' target='_blank'>Hao Yang's Group</a>. All Rights Reserved.")),
          div(style="text-align:center;margin-bottom: 20px;font-size:100%",
              HTML("&nbsp;&nbsp; Created by Shisheng Wang. E-mail: <u>wsslearning@omicsolution.com</u>."))
        ),
        column(3)
      )
    )
  })
  examplepeakdatas<-reactive({
    if(input$datatypex==1){
      dataread<-read.csv("Phospho_Exampledata1.csv",stringsAsFactors = F,check.names = F)
    }
    else if(input$datatypex==2){
      dataread<-read.csv("Phospho_Exampledata2.csv",stringsAsFactors = F,check.names = F)
    }
    else if(input$datatypex==3){
      dataread<-read.csv("Phospho_Exampledata3.csv",stringsAsFactors = F,check.names = F)
    }
    else if(input$datatypex==4){
      dataread<-read.csv("Pro_Exampledata.csv",stringsAsFactors = F,check.names = F)
    }
    else{
      dataread<-read.csv("Pro_Exampledata.csv",stringsAsFactors = F,check.names = F)
    }
    dataread
  })
  examplesampledatas<-reactive({
    if(input$datatypex==1 | input$datatypex==2 | input$datatypex==3){
      dataread<-read.csv("grinfo.csv",header = T,stringsAsFactors = F,check.names = F)
    }else{
      dataread<-read.csv("grinfo2.csv",header = T,stringsAsFactors = F,check.names = F)
    }
    colnames(dataread)<-c("Samples","Groups")
    dataread
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = FALSE)
    }
  )
  output$loaddatadownload2<-downloadHandler(
    filename = function(){paste("Example_SampleData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplesampledatas(),file,row.names = FALSE)
    }
  )
  peaksdataout<-reactive({
    files <- input$file1
    if (is.null(files)){
      dataread<-data.frame(Description="NAguideR detects that you did not upload your data. Please upload the expression data, or load the example data to check first.")
      list(yuanshidf=dataread)
    }else{
      if (input$fileType_Input == "3"){
        dataread<-read.xlsx(files$datapath,rowNames=input$firstcol,
                            colNames = input$header,sheet = input$xlsxindex)
      }
      else if(input$fileType_Input == "2"){
        if(sum(input$firstcol)==1){
          rownametf<-1
        }else{
          rownametf<-NULL
        }
        dataread<-read.xls(files$datapath,sheet = input$xlsxindex,header=input$header,
                           row.names = rownametf, sep=input$sep,stringsAsFactors = F)
      }
      else{
        if(sum(input$firstcol)==1){
          rownametf<-1
        }else{
          rownametf<-NULL
        }
        dataread<-read.csv(files$datapath,header=input$header,
                           row.names = rownametf, sep=input$sep,stringsAsFactors = F)
      }
      if(input$datatypex==1){
        dataread1<-dataread[,-c(1,2)]
        dataread2<-dataread[,c(1,2)]
      }
      else if(input$datatypex==2){
        dataread1<-dataread[,-c(1,2)]
        dataread2<-dataread[,c(1,2)]
      }
      else if(input$datatypex==3){
        dataread1<-dataread[,-c(1,2,3)]
        dataread2<-dataread[,c(1,2,3)]
      }
      else if(input$datatypex==4){
        dataread1<-dataread[,-1]
        dataread2<-dataread[,1,drop=FALSE]
      }
      else{
        dataread1<-dataread[,-1]
        dataread2<-dataread[,1,drop=FALSE]
      }
      rowpaste<-apply(dataread2,1,function(x){
        paste0(x,collapse = "_")
      })
      dataread1x<-dataread1[!duplicated(rowpaste),]
      rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
      list(yuanshidf=dataread,yuanshidata=dataread1x,objectinfo=dataread2)
    }
  })
  samplesdataout<-reactive({
    files <- input$mchuanshaodxyibanfile1_fenzu
    if (is.null(files)){
      dataread<-data.frame(Description="NAguideR detects that you did not upload your data. Please upload the sample information data, or load the example data to check first.")
    }else{
      if (input$mchuanshaodxyibanfileType_Input_fenzu == "3"){
        dataread<-read.xlsx(files$datapath,rowNames=input$mchuanshaodxyibanfirstcol_fenzu,
                            colNames = input$mchuanshaodxyibanheader_fenzu,sheet = input$mchuanshaodxyibanxlsxindex_fenzu)
      }
      else if(input$mchuanshaodxyibanfileType_Input_fenzu == "2"){
        if(sum(input$mchuanshaodxyibanfirstcol_fenzu)==1){
          rownametfmchuanshaodxyiban_fenzu<-1
        }else{
          rownametfmchuanshaodxyiban_fenzu<-NULL
        }
        dataread<-read.xls(files$datapath,sheet = input$mchuanshaodxyibanxlsxindex_fenzu,header=input$mchuanshaodxyibanheader_fenzu,
                           row.names = rownametfmchuanshaodxyiban_fenzu, sep=input$mchuanshaodxyibansep_fenzu,stringsAsFactors = F)
      }
      else{
        if(sum(input$mchuanshaodxyibanfirstcol_fenzu)==1){
          rownametfmchuanshaodxyiban_fenzu<-1
        }else{
          rownametfmchuanshaodxyiban_fenzu<-NULL
        }
        dataread<-read.csv(files$datapath,header=input$mchuanshaodxyibanheader_fenzu,
                           row.names = rownametfmchuanshaodxyiban_fenzu, sep=input$mchuanshaodxyibansep_fenzu,stringsAsFactors = F)
      }
      colnames(dataread)<-c("Samples","Groups")
    }
    dataread
  })

  output$peaksdata<-renderDataTable({
    library(data.table)
    aaxxc<-peaksdataout()
    if(input$loaddatatype==1){
      datatable(peaksdataout()$yuanshidf, options = list(pageLength = 10))
    }else{
      datatable(examplepeakdatas(), options = list(pageLength = 10))
    }
  })

  output$samplesdata<-renderDataTable({
    if(input$loaddatatype==1){
      samplesdf<-samplesdataout()
    }else{
      samplesdf<-examplesampledatas()
    }

    if(input$loaddatatype==1){
      datatable(samplesdf, options = list(pageLength = 10))
    }else{
      datatable(examplesampledatas(), options = list(pageLength = 10))
    }
  })
  ##
  nadataout<-reactive({
    if(input$loaddatatype==1){
      nadatax<-peaksdataout()$yuanshidata
    }else{
      dataread<-examplepeakdatas()
      if(input$datatypex==1){
        dataread1<-dataread[,-c(1,2)]
        dataread2<-dataread[,c(1,2)]
      }
      else if(input$datatypex==2){
        dataread1<-dataread[,-c(1,2)]
        dataread2<-dataread[,c(1,2)]
      }
      else if(input$datatypex==3){
        dataread1<-dataread[,-c(1,2,3)]
        dataread2<-dataread[,c(1,2,3)]
      }
      else if(input$datatypex==4){
        dataread1<-dataread[,-1]
        dataread2<-dataread[,1,drop=FALSE]
      }
      else{
        dataread1<-dataread[,-1]
        dataread2<-dataread[,1,drop=FALSE]
      }
      rowpaste<-apply(dataread2,1,function(x){
        paste0(x,collapse = "_")
      })
      dataread1x<-dataread1[!duplicated(rowpaste),]
      rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
      nadatax<-dataread1x
    }

    nadatax[nadatax==input$natype]<-NA
    nadatax[] <- lapply(nadatax, function(x) as.numeric(as.character(x)))
    nadatax
  })
  plot_missing_xiu<-function (data, title = NULL){
    feature <- num_missing <- pct_missing <- group <- NULL
    is_data_table <- is.data.table(data)
    data_class <- class(data)
    if (!is_data_table) {
      data <- data.table(data)
    }
    missing_value <- data.table(feature = names(data), num_missing = sapply(data,function(x) {sum(is.na(x))}))
    missing_value[, `:=`(feature, factor(feature, levels = feature[order(-rank(num_missing))]))]
    missing_value[, `:=`(pct_missing, num_missing/nrow(data))]
    missing_value[pct_missing < 0.2, `:=`(group, "Good")]
    missing_value[pct_missing >= 0.2 & pct_missing < 0.6, `:=`(group,"OK")]
    missing_value[pct_missing >= 0.6 & pct_missing < 0.8, `:=`(group,"Bad")]
    missing_value[pct_missing >= 0.8, `:=`(group, "Remove")]
    if (!is_data_table) {
      class(missing_value) <- data_class
    }
    output <- ggplot(missing_value, aes_string(x = "feature",y = "num_missing", fill = "group")) +
      geom_bar(stat = "identity",colour = "black", alpha = 0.7) +
      geom_text(aes(label = paste0(round(100 *pct_missing, 2), "%")), hjust = -0.15, size = 4) +
      scale_fill_manual("Group", values = c(Good = "#1FB04C",OK = "#9AD94F", Bad = "#FDA249", Remove = "#D71316"),
                        breaks = c("Good", "OK", "Bad", "Remove")) + coord_flip() +
      xlab("Features") + ylab("Number of missing rows") +
      ylim(c(0,max(missing_value$num_missing)+10))+
      ggtitle(title)
    output
  }
  preheightx<-reactive({
    input$preheight
  })
  observeEvent(
    input$mcsbtn_nafenbu,{
      shinyjs::show(id = "mcsbtn_nafenbu_hid", anim = FALSE)
      output$nadatadf<-renderDataTable({
        datatable(nadataout(), options = list(pageLength = 20))
      })
      output$nadatadfdl<-downloadHandler(
        filename = function(){paste("NAdata_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(nadataout(),file)
        }
      )
      output$naplotbycolumn<-renderPlot({
        dfx<-nadataout()
        plot_missing_xiu(dfx)+theme_bw()+
          theme(legend.position = c("bottom"),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = preheightx)
      naplotbycolumnout<-reactive({
        dfx<-nadataout()
        plot_missing_xiu(dfx)+theme_bw()+
          theme(legend.position = c("bottom"),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$naplotbycolumndl<-downloadHandler(
        filename = function(){paste("NAplot_bycolumn_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = preheightx()/100+1,height = preheightx()/100+1)
          print(naplotbycolumnout())
          dev.off()
        }
      )
      output$naplotbyrow<-renderPlot({
        library(Amelia)
        natypecolx<-strsplit("#A11315;#8C86FF",";|,")[[1]]#input$natypecol
        dfx<-nadataout()
        tdatamaxqproNA<-as.data.frame(t(dfx))
        colnames(tdatamaxqproNA)<-paste0("Row ",1:ncol(tdatamaxqproNA))
        missmap(tdatamaxqproNA,y.labels=rev(colnames(dfx)),x.cex = 0.5, col = natypecolx)
      },height = preheightx)
      naplotbyrowout<-reactive({
        natypecolx<-strsplit("#A11315;#8C86FF",";|,")[[1]]
        dfx<-nadataout()
        tdatamaxqproNA<-as.data.frame(t(dfx))
        colnames(tdatamaxqproNA)<-paste0("Row ",1:ncol(tdatamaxqproNA))
        missmap(tdatamaxqproNA,y.labels=rev(colnames(dfx)),x.cex = 0.5, col = natypecolx)
      })
      output$naplotbyrowdl<-downloadHandler(
        filename = function(){paste("NAplot_byrow_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = preheightx()/100+1,height = preheightx()/100+1)
          print(naplotbyrowout())
          dev.off()
        }
      )

    }
  )
  #
  filtereddatadfout<-reactive({
    if(input$loaddatatype==1){
      grdfx<-samplesdataout()
    }else{
      grdfx<-examplesampledatas()
    }
    datadf<-nadataout()
    grnames<-unique(grdfx[[2]])
    naratiox<-input$naratio
    if(input$fenzukaolvif){
      nastatsdf<-NULL
      nastatsdf0<-NULL
      for(i in 1:length(grnames)){
        dataindex<-datadf[,grdfx[[2]]==grnames[i]]
        narowsumix<-apply(dataindex,1,function(x){sum(is.na(x))})==ncol(dataindex)
        narowsumi<-apply(dataindex,1,function(x){sum(is.na(x))})/ncol(dataindex)
        dataindex1<-data.frame(nabili=narowsumi<=naratiox)
        nastatsdf<-cbind(nastatsdf,as.matrix(dataindex1))
        nastatsdf0<-cbind(nastatsdf0,as.matrix(dataindex))
      }
      nafenzuindex<-apply(nastatsdf,1,function(x){
        if(all(x)){
          return(TRUE)
        }else{
          return(FALSE)
        }
      })
      datadfchuli<-datadf[nafenzuindex,]
      datadfchuli<-as.data.frame(datadfchuli)
    }else{
      narowsum<-apply(datadf,1,function(x){sum(is.na(x))})/ncol(datadf)
      datadfchuli<-datadf[narowsum<=input$naratio,]
    }
    datadfchulix<-datadfchuli
    if(input$mediannormif){
      medianval<-apply(datadfchuli,2,function(x) {median(x,na.rm = TRUE)})
      datadfchuli<-sweep(datadfchuli,2,medianval,FUN = "/")
    }
    if(input$logif){
      datadfchuli<-log2(datadfchuli)
    }

    list(datadfchuli=datadfchuli,datadfchuli_nonorm=datadfchulix)
  })
  cvfilterdataout<-reactive({
    dfxx<-filtereddatadfout()$datadfchuli
    dfx1<-filtereddatadfout()$datadfchuli_nonorm
    if(input$mediannormif){
      medianval<-apply(dfx1,2,function(x) {median(x,na.rm = TRUE)})
      dfx<-sweep(dfx1,2,medianval,FUN = "/")
    }else{
      dfx<-dfx1
    }
    if(input$loaddatatype==1){
      samplesdf<-samplesdataout()
    }else{
      samplesdf<-examplesampledatas()
    }
    grnames<-unique(samplesdf$Groups)
    cvdf<-cvifdf<-NULL
    for(i in 1:length(grnames)){
      datai<-dfx[,samplesdf$Groups==grnames[i]]
      cvi<-apply(datai,1,function(x){
        if(all(as.numeric(x)==0)){
          0
        }else{
          raster::cv(as.numeric(x),na.rm = TRUE)
        }
      })
      cvdf<-cbind(cvdf,cvi)
      cvifdf<-cbind(cvifdf,cvi>input$cvyuzhi*100)
    }
    cvifdf1<-apply(cvifdf,1,any)
    dfxx1<-dfxx[!cvifdf1,]
    dfxx00<-apply(dfxx1, 1, sum,na.rm = TRUE)
    dfxx2<-dfxx1[dfxx00!=0,]
    round(dfxx2,digits=5)
  })
  datanonaout<-reactive({
    dfx<-cvfilterdataout()
    dfx<-dfx[complete.cases(dfx),]
    dfx
  })
  datahasnaout<-reactive({
    dfx<-cvfilterdataout()
    dfx<-dfx[!complete.cases(dfx),]
    dfx
  })

  observeEvent(
    input$mcsbtn_nafilter,{
      shinyjs::show(id = "mcsbtn_nafilter_hid", anim = FALSE)
      output$filtereddatadf<-renderDataTable({
        datatable(cvfilterdataout(), options = list(pageLength = 20))
      })
      output$filtereddatadfdl<-downloadHandler(
        filename = function(){paste("Filtereddata_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(cvfilterdataout(),file)
        }
      )
      output$datanona<-renderDataTable({
        datatable(datanonaout(), options = list(pageLength = 20))
      })
      output$datanonadl<-downloadHandler(
        filename = function(){paste("DataWithoutNA_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(datanonaout(),file)
        }
      )
      output$datahasna<-renderDataTable({
        datatable(datahasnaout(), options = list(pageLength = 20))
      })
      output$datahasnadl<-downloadHandler(
        filename = function(){paste("DataWithNA_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(datahasnaout(),file)
        }
      )
    }
  )
  output$inputdatacheck1<-renderUI({
    yuanshiinputdata<-nadataout()
    filtereddata<-cvfilterdataout()
    div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:130%;color:black",
        HTML("~~ <em>Check information for input data</em> ~~<br />"),
        HTML(paste0("&nbsp;&nbsp;1. There are ",nrow(yuanshiinputdata)," rows and ",ncol(yuanshiinputdata),
                    " columns in the input expression data;<br />")),
        HTML(paste0("&nbsp;&nbsp;2. After removing those rows with high proportion of missing values and coefficient of variation
                    (the threshold can be set on the left parameter panel), there are ",
                    nrow(filtereddata)," rows left in the filtered data;<br />")))
  })
  output$inputdatacheck2<-renderUI({
    yuanshiinputdata<-nadataout()
    filtereddata<-cvfilterdataout()
    if(nrow(filtereddata)/nrow(yuanshiinputdata)>0.5){
      div(style="text-align:left;margin-top:10px;margin-right:150px;font-size:150%;color:green",
          HTML(paste0("Finally, ",round(nrow(filtereddata)/nrow(yuanshiinputdata),2)*100," % of the input data are retained, NAguideR thinks this is acceptable.
                      However, if you do not think so, please check your input data and the parameters, you can adjust them on the left panel and then proceed to the next step.")))
    }else{
      div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:150%;color:red",
          HTML(paste0("Warning: ",round(1-nrow(filtereddata)/nrow(yuanshiinputdata),2)*100," % of the input data are removed, NAguideR suggests you check or adjust your
                      input data and the parameters again. If you can be sure there are no problems on the input data and parameters, you can proceed to the next step.")))
    }
  })
  #
  suijidataout<-reactive({
    dfx<-cvfilterdataout()
    naratiox<-dim(dfx[!complete.cases(dfx),])[1]/nrow(dfx)
    nacolratio<-apply(dfx[!complete.cases(dfx),],2,function(x){sum(is.na(x))})/nrow(dfx[!complete.cases(dfx),])
    datanonaoutx<-datanonaout()
    nanum<-round(naratiox*nrow(datanonaoutx))
    set.seed(123)
    samplenaindex<-sample(1:nrow(datanonaoutx),nanum)
    datanonaoutxx1<-datanonaoutx
    for(i in 1:ncol(datanonaoutx)){
      set.seed(i)
      samplenaindexi<-sample(samplenaindex,round(nacolratio[i]*nanum))
      datanonaoutxx1[samplenaindexi,i]<-NA
    }
    if(nrow(datanonaoutxx1)!=sum(complete.cases(datanonaoutxx1))){
      datanonaoutxx1_1<-datanonaoutx[samplenaindex,]
      datanonaoutxx1_2<-datanonaoutx[-samplenaindex,]
      for(ii in 1:nrow(datanonaoutxx1_1)){
        set.seed(ii)
        eachrownaratio<-round(naratiox*ncol(datanonaoutx))
        samplenaindexi<-sample(1:ncol(datanonaoutx),ifelse(eachrownaratio==0,round(0.3*ncol(datanonaoutx)),eachrownaratio))
        datanonaoutxx1_1[ii,samplenaindexi]<-NA
      }
      datanonaoutxx1<-rbind(datanonaoutxx1_1,datanonaoutxx1_2)
      datanonaoutxx1<-datanonaoutxx1[match(rownames(datanonaoutx), rownames(datanonaoutxx1)),]
    }
    rowcolindex1<-which(is.na(datanonaoutxx1),arr.ind = TRUE)
    phosdata_na_sim1<-datanonaoutxx1
    list(suijinadatadf=phosdata_na_sim1,rowcolindex=rowcolindex1)
  })
  namethodsoutxx<-reactive({
    namethods<-vector()
    if(input$zeromethodif) namethods[1]<-"zero"
    if(input$minimummethodif) namethods[2]<-"minimum"
    if(input$colmedianif) namethods[3]<-"colmedian"
    if(input$rowmedianif) namethods[4]<-"rowmedian"
    if(input$mindetif) namethods[5]<-"mindet"
    if(input$minprobif) namethods[6]<-"minprob"
    if(input$PIif) namethods[7]<-"PI"
    if(input$svdmethodif) namethods[8]<-"svdmethod"
    if(input$mleif) namethods[9]<-"mle"
    if(input$impseqif) namethods[10]<-"impseq"
    if(input$impseqrobif) namethods[11]<-"impseqrob"
    if(input$bpcaif) namethods[12]<-"bpca"
    if(input$KNNmethodif) namethods[13]<-"KNNmethod"
    if(input$seqknnif) namethods[14]<-"seqknn"
    if(input$QRILCif) namethods[15]<-"QRILC"
    if(input$llsif) namethods[16]<-"lls"
    if(input$GRRif) namethods[17]<-"GRR"
    if(input$micenormif) namethods[18]<-"mice-norm"
    if(input$trknnif) namethods[19]<-"trknn"
    if(input$irmif) namethods[20]<-"irm"
    if(input$GMSif) namethods[21]<-"GMS"
    if(input$micecartif) namethods[22]<-"mice-cart"
    if(input$RFif) namethods[23]<-"RF"
    namethods
  })
  namethodsout<-eventReactive(input$mcsbtn_imputationII,{
    namethodsoutxx()
  })
  output$suijidataresultsui<-renderUI({
    namethodsoutx<-tolower(na.omit(namethodsout()))
    selectInput("suijijieguoindex","",
                choices = namethodsoutx)
  })
  nafunctions<-function(x,method="zero"){
    df<-df1<-as.data.frame(x)
    method<-tolower(method)
    if(method=="zero"){
      df[is.na(df)]<-0
    }
    else if(method=="minimum"){
      df[is.na(df)]<-min(df1,na.rm = TRUE)
    }
    else if(method=="colmedian"){
      library(e1071)
      df<-impute(df1,what ="median")
    }
    else if(method=="rowmedian"){
      library(e1071)
      dfx<-impute(t(df1),what ="median")
      df<-t(dfx)
    }
    else if(method=="knnmethod"){
      library(impute)
      data_zero1<-impute.knn(as.matrix(df1),k = 10, rowmax = 0.9, colmax = 0.9)
      df<-data_zero1$data
    }
    else if(method=="seqknn"){
      library(SeqKnn)
      df <- SeqKNN(df1,k = 10)
    }
    else if(method=="bpca"){
      library(pcaMethods)
      data_zero1<-pcaMethods::pca(as.matrix(df1), nPcs = ncol(df1)-1, method = "bpca", maxSteps =100)
      df<-completeObs(data_zero1)
    }
    else if(method=="svdmethod"){
      library(pcaMethods)
      data_zero1<-pcaMethods::pca(as.matrix(df1), nPcs = ncol(df1)-1, method = "svdImpute")
      df<-completeObs(data_zero1)
    }
    else if(method=="lls"){
      library(pcaMethods)
      data_zero1<-llsImpute(t(df1), k = 10)
      df<-t(completeObs(data_zero1))
    }
    else if(method=="mle"){
      library(norm)
      xxm<-as.matrix(df1)
      ss <- norm::prelim.norm(xxm)
      thx <- norm::em.norm(ss)
      norm::rngseed(123)
      df <- norm::imp.norm(ss, thx, xxm)
    }
    else if(method=="qrilc"){
      library(imputeLCMD)
      xxm<-t(df1)
      data_zero1 <- imputeLCMD::impute.QRILC(xxm, tune.sigma = 1)[[1]]
      df<-t(data_zero1)
    }
    else if(method=="mindet"){
      library(imputeLCMD)
      xxm<-as.matrix(df1)
      df <- imputeLCMD::impute.MinDet(xxm, q = 0.01)
    }
    else if(method=="minprob"){
      library(imputeLCMD)
      xxm<-as.matrix(df1)
      df <- imputeLCMD::impute.MinProb(xxm, q = 0.01, tune.sigma = 1)
    }
    else if(method=="irm"){
      library(VIM)
      df <- irmi(df1, trace = TRUE,imp_var=FALSE)
      rownames(df)<-rownames(df1)
    }
    else if(method=="impseq"){
      library(rrcovNA)
      df <- impSeq(df1)
    }
    else if(method=="impseqrob"){
      library(rrcovNA)
      data_zero1 <- impSeqRob(df1, alpha=0.9)
      df<-data_zero1$x
    }
    else if(method=="mice-norm"){
      library(mice)
      minum<-5
      datareadmi<-mice(df1,m=minum,seed = 1234, method ="norm")
      newdatareadmi<-0
      for (i in 1:minum) {
        newdatareadmi<-complete(datareadmi,action = i)+newdatareadmi
      }
      df<-newdatareadmi/minum
      rownames(df)<-rownames(df1)
    }
    else if(method=="mice-cart"){
      library(mice)
      minum<-5
      datareadmi<-mice(df1,m=minum,seed = 1234, method ="cart")
      newdatareadmi<-0
      for (i in 1:minum) {
        newdatareadmi<-complete(datareadmi,action = i)+newdatareadmi
      }
      df<-newdatareadmi/minum
      rownames(df)<-rownames(df1)
    }
    else if(method=="trknn"){
      source('Trunc_KNN/Imput_funcs.r')
      sim_trKNN_wrapper <- function(data) {
        result <- data %>% as.matrix %>% t %>% imputeKNN(., k=10, distance='truncation', perc=0) %>% t
        return(result)
      }
      df1x <- sim_trKNN_wrapper(t(df1))
      df<-as.data.frame(t(df1x))
    }
    else if(method=="rf"){
      library(missForest)
      data_zero1 <- missForest(t(df1), maxiter =10,ntree = input$rfntrees,mtry=floor(row(df1)^(1/3)),verbose = TRUE)
      df<-t(data_zero1$ximp)
    }
    else if(method=="pi"){
      width <- input$piwidth
      downshift <- input$pidownshift
      for(i in 1:ncol(df1)){
        temp <- df1[[i]]
        if(sum(is.na(temp))>0){
          temp.sd <- width * sd(temp[!is.na(temp)], na.rm = TRUE)
          temp.mean <- mean(temp[!is.na(temp)], na.rm = TRUE) - downshift * sd(temp[!is.na(temp)], na.rm = TRUE)
          n.missing <- sum(is.na(temp))
          temp[is.na(temp)] <- rnorm(n.missing, mean = temp.mean, sd = temp.sd)
          df[[i]]<-temp
        }
      }
      df
    }
    else if(method=="grr"){
      library(DreamAI)
      df<-impute.RegImpute(data=as.matrix(df1), fillmethod = "row_mean", maxiter_RegImpute = 10,conv_nrmse = 1e-03)
    }
    else if(method=="gms"){
      library(GMSimpute)
      df<-GMS.Lasso(df1,nfolds=3,log.scale=FALSE,TS.Lasso=TRUE)
    }
    else{
      stop("Unspported methods so far~~")
    }
    df<-as.data.frame(df)
    df
  }
  suijidataimputeout<-reactive({
    namethodsoutx<-tolower(na.omit(namethodsout()))
    namethodsoutx1<-c(namethodsoutx,"Finish")
    suijinadatadfx<-suijidataout()$suijinadatadf
    rowcolindexx<-suijidataout()$rowcolindex
    withProgress(message = 'Imputing data with ', style = "notification", detail = paste0("1 ",namethodsoutx[1]), value = 0,{
      suijidataimputelist<-list()
      for(i in 1:length(namethodsoutx)){
        suijidataimputelist[[i]]<-nafunctions(suijinadatadfx,method=namethodsoutx[i])
        incProgress(1/length(namethodsoutx), detail = paste0(i," ",namethodsoutx[i]," Done ~",
                                                             namethodsoutx1[i+1]," processing..."))
      }
    })
    names(suijidataimputelist)<-tolower(namethodsoutx)
    suijidataimputelist
  })
  nrmsedfout<-reactive({
    datanonaoutx<-datanonaout()
    namethodsoutx<-tolower(na.omit(namethodsout()))
    suijinadatadfx<-suijidataout()$suijinadatadf
    rowcolindexx<-suijidataout()$rowcolindex
    suijidatajieguo<-suijidataimputeout()
    nrmsei<-vector()
    for(i in 1:length(namethodsoutx)){
      impdata<-as.numeric(suijidatajieguo[[namethodsoutx[i]]][rowcolindexx])
      truedata<-as.numeric(datanonaoutx[rowcolindexx])
      nrmsejisuan<-sqrt(mean((impdata - truedata)^{2})/var(truedata))
      nrmsei[i]<-nrmsejisuan
    }
    nrmsedf<-data.frame(Methods=namethodsoutx,NRMSE=round(nrmsei,digits=5),stringsAsFactors = FALSE)
    nrmsedf<-nrmsedf[order(nrmsedf$NRMSE),]
    rownames(nrmsedf)<-paste0("Method ",rownames(nrmsedf))
    nrmsedf
  })
  sornrmsedfout<-reactive({
    datanonaoutx<-datanonaout()
    namethodsoutx<-tolower(na.omit(namethodsout()))
    suijinadatadfx<-suijidataout()$suijinadatadf
    rowcolindexx<-suijidataout()$rowcolindex
    suijidatajieguo<-suijidataimputeout()
    var_idx_temp<-sort(unique(rowcolindexx[,1]))
    nrmserank<-0
    for (j in var_idx_temp) {
      nrmsei<-vector()
      colnaindex<-is.na(as.numeric(suijinadatadfx[j,]))
      for (k in 1:length(namethodsoutx)) {
        impdata<-as.numeric(suijidatajieguo[[namethodsoutx[k]]][j,colnaindex])
        truedata<-as.numeric(datanonaoutx[j,colnaindex])
        nrmsejisuan<-Metrics::rmse(impdata,truedata)
        nrmsei[k]<-nrmsejisuan
      }
      nrmserank <- nrmserank+rank(nrmsei)
    }
    nrmserankdf<-data.frame(Methods=namethodsoutx,SOR=round(nrmserank,digits=5),stringsAsFactors = FALSE)
    nrmserankdf<-nrmserankdf[order(nrmserankdf$SOR),]
    rownames(nrmserankdf)<-paste0("Method ",rownames(nrmserankdf))
    nrmserankdf
  })
  avgcorout<-reactive({
    datanonaoutx<-datanonaout()
    namethodsoutx<-tolower(na.omit(namethodsout()))
    suijinadatadfx<-suijidataout()$suijinadatadf
    rowcolindexx<-suijidataout()$rowcolindex
    suijidatajieguo<-suijidataimputeout()
    var_idx_temp<-sort(unique(rowcolindexx[,1]))
    avgcorxx<-0
    for(i in var_idx_temp){
      avgcorx<-vector()
      for(j in 1:length(namethodsoutx)){
        avgcorx[j]<-cor(as.numeric(datanonaoutx[i,]),as.numeric(suijidatajieguo[[namethodsoutx[j]]][i,]))
      }
      avgcorxx<-avgcorxx+avgcorx
    }
    avgcorxxdf<-data.frame(Methods=namethodsoutx,Cor_mean=round(avgcorxx/length(var_idx_temp),digits=5),stringsAsFactors = FALSE)
    avgcorxxdf<-avgcorxxdf[order(avgcorxxdf$Cor_mean,decreasing = TRUE),]
    rownames(avgcorxxdf)<-paste0("Method ",rownames(avgcorxxdf))
    avgcorxxdf
  })
  pssresout<-reactive({
    datanonaoutx<-datanonaout()
    pcayuanshi1 <- summary(prcomp(t(datanonaoutx), scale.=T, center=T))
    pcazhanbi<-pcayuanshi1$importance
    pcazhanbi95<-which(as.numeric(pcazhanbi[3,])>=0.95)[1]
    pcayuanshi <- prcomp(t(datanonaoutx), scale.=T, center=T)$x[, 1:pcazhanbi95]
    namethodsoutx<-tolower(na.omit(namethodsout()))
    suijidatajieguo<-suijidataimputeout()
    pssresoutx<-vector()
    for(i in 1:length(namethodsoutx)){
      pcasuijiresi<-prcomp(t(suijidatajieguo[[namethodsoutx[i]]]), scale.=T, center=T)$x[, 1:pcazhanbi95]
      pssresoutx[i]<- vegan::procrustes(pcayuanshi, pcasuijiresi, symmetric=T)$ss
    }
    pssresdf<-data.frame(Methods=namethodsoutx,PSS=round(pssresoutx,digits=5),stringsAsFactors = FALSE)
    pssresdf<-pssresdf[order(pssresdf$PSS),]
    rownames(pssresdf)<-paste0("Method ",rownames(pssresdf))
    pssresdf
  })
  pinjiafigheightx<-reactive({
    input$pinjiafigheight
  })
  assessrankout<-reactive({
    nrmsedf<-nrmsedfout()
    nrmsedf$NRMSE_Rank<-rank(nrmsedf$NRMSE)
    sornrmsedf<-sornrmsedfout()
    sornrmsedf$SOR_Rank<-rank(sornrmsedf$SOR)
    avgcor<-avgcorout()
    avgcor$ACC_OI_Rank<-rank(1/avgcor$Cor_mean)
    pssres<-pssresout()
    pssres$PSS_Rank<-rank(pssres$PSS)
    rankdf<-Reduce(function(x, y) merge(x, y, all=TRUE), list(nrmsedf, sornrmsedf, avgcor,pssres))#,accpcresdf
    rankdf1<-rankdf[,c(1,3,5,7,9)]
    #
    if(input$customclassiccriteriaif){
      xuanzecc<-paste0(input$classiccriteriaxuabze,"_Rank")
      xuanzeccweight<-as.numeric(strsplit(input$classiccriteriaweight,";")[[1]])
      rankdf2<-as.matrix(rankdf1[,xuanzecc,drop=TRUE])
      rankdf1$Rank_Mean<-rankdf2%*%c(xuanzeccweight/sum(xuanzeccweight))
      rankdf1<-rankdf1[,c("Methods",xuanzecc,"Rank_Mean")]
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }else{
      rankdf1$Rank_Mean<-rowMeans(rankdf[,c(3,5,7,9)])
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }
    rankdf1[order(rankdf1$Rank_Mean),]
  })
  imputeIplotdataout<-reactive({
    datanonaoutx<-datanonaout()
    suijinadatadfx<-suijidataout()$suijinadatadf
    suijidatajieguo<-suijidataimputeout()
    namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
    dataoutx<-suijidatajieguo[[namethodsx]]
    datanonaoutx1<-datanonaoutx[!complete.cases(suijinadatadfx),]
    dataoutx1<-dataoutx[!complete.cases(suijinadatadfx),]
    queshidf<-suijinadatadfx[!complete.cases(suijinadatadfx),]
    if(input$loaddatatype==1){
      samplesdf<-samplesdataout()
    }else{
      samplesdf<-examplesampledatas()
    }
    grnames<-unique(samplesdf$Groups)
    cv_original<-cv_imputation<-vector()
    k<-1
    for(i in 1:nrow(datanonaoutx1)){
      for(j in 1:length(grnames)){
        Anumj1<-as.numeric(datanonaoutx1[i,samplesdf$Groups==grnames[j]])
        Anumj2<-as.numeric(dataoutx1[i,samplesdf$Groups==grnames[j]])
        Anumj3<-as.numeric(queshidf[i,samplesdf$Groups==grnames[j]])
        if(sum(is.na(Anumj3))>0){
          cv_original[k]<-sd(Anumj1)/mean(Anumj1)
          cv_imputation[k]<-sd(Anumj2)/mean(Anumj2)
          k<-k+1
        }
      }
    }
    cvqianhoudf2<-data.frame(Index=rep(1:length(cv_original),length(grnames)),
                             CV_group=c(rep("cv_original",length(cv_original)),rep("cv_imputation",length(cv_original))),
                             CV=c(cv_original,cv_imputation),stringsAsFactors = FALSE)
    aax1<-as.numeric(as.matrix(datanonaoutx))
    aax2<-as.numeric(as.matrix(dataoutx))
    aax3<-as.numeric(as.matrix(suijinadatadfx))
    grx<-rep("Original",length(aax1))
    grx[is.na(aax3)]<-"Imputation"
    pointrugdf<-pointrugdfx<-data.frame(Groups=grx,Original_Values=aax1,Imputation_Values=aax2,stringsAsFactors = FALSE)
    if(nrow(pointrugdfx)>=100000){
      pointrugdf1<-pointrugdf[pointrugdf$Groups=="Imputation",]
      pointrugdf2<-pointrugdf[pointrugdf$Groups!="Imputation",]
      set.seed(123)
      pointrugdf<-rbind(pointrugdf1,pointrugdf2[sample(1:nrow(pointrugdf2),0.1*nrow(pointrugdf2)),])
    }
    if(nrow(pointrugdfx)>30000 & nrow(pointrugdfx)<100000){
      pointrugdf1<-pointrugdf[pointrugdf$Groups=="Imputation",]
      pointrugdf2<-pointrugdf[pointrugdf$Groups!="Imputation",]
      set.seed(123)
      pointrugdf<-rbind(pointrugdf1,pointrugdf2[sample(1:nrow(pointrugdf2),0.2*nrow(pointrugdf2)),])
    }
    list(cvqianhoudf2=cvqianhoudf2,pointrugdf=pointrugdf)
  })
  #######################
  observeEvent(
    input$mcsbtn_imputation,{
      shinyjs::show(id = "mcsbtn_imputation_hid", anim = FALSE)
      output$suijidataresults<-renderDataTable({
        suijidatajieguo<-suijidataimputeout()
        namethodsx<-tolower(input$topnmethodindex)
        dataoutx<-suijidatajieguo[[namethodsx]]
        datatable(dataoutx, options = list(pageLength = 20))
      })
      output$imputeIplot<-renderPlot({
        cvqianhoudf2<-imputeIplotdataout()$cvqianhoudf2
        namethodsx<-tolower(input$topnmethodindex)
        withProgress(message = 'Figure ', style = "notification", detail = "generating...", max =2, value = 1,{
          ppx1<-ggplot(cvqianhoudf2, aes(x=Index, y=CV)) +
            geom_line(aes(group=Index),col="grey80",size=1)+
            geom_point(aes(col=CV_group),size=3,alpha=0.6)+
            geom_smooth(method="auto", aes(fill=CV_group,col=CV_group),
                        lty=2, show.legend =TRUE,alpha=0.85)+
            scale_color_manual(name="Groups",values = c("#DA70D6","#7B68EE"))+
            scale_fill_manual(name="Groups",values = c("maroon","royalblue"))+
            labs(x="Index",y="CV values",title = paste0("Method: ",namethodsx))+
            theme_bw()+
            theme(legend.position = "bottom",axis.text=element_text(size=14),
                  axis.title=element_text(size=16),legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14))#"maroon","royalblue"
          pointrugdf<-imputeIplotdataout()$pointrugdf
          ppx<-ggplot(pointrugdf, aes(Imputation_Values, Original_Values,col=Groups)) +
            ggrastr::geom_point_rast() +
            scale_color_manual(values = c("#DA70D6","#7B68EE"))+
            theme_bw()+
            theme(legend.position = "bottom",axis.text=element_text(size=14),
                  axis.title=element_text(size=16),legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14))
          ppx2<-ggMarginal(ppx, groupColour = TRUE, groupFill = TRUE, xparams = list(bins = 30,position ="dodge"),
                           yparams = list(bins = 30,position ="dodge"), type = "histogram")
          plot_grid(ppx1,ppx2,nrow=1,ncol=2)
        })
      },height = pinjiafigheightx)
      imputeIplotout<-reactive({
        cvqianhoudf2<-imputeIplotdataout()$cvqianhoudf2
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
        ppx1<-ggplot(cvqianhoudf2, aes(x=Index, y=CV)) +
          geom_line(aes(group=Index),col="grey80",size=1)+
          geom_point(aes(col=CV_group),size=3,alpha=0.6)+
          geom_smooth(method="auto", aes(fill=CV_group,col=CV_group),
                      lty=2, show.legend =TRUE,alpha=0.85)+
          scale_color_manual(name="Groups",values = c("#DA70D6","#7B68EE"))+
          scale_fill_manual(name="Groups",values = c("maroon","royalblue"))+
          labs(x="Index",y="CV values",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position = "bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))#"maroon","royalblue"
        pointrugdf<-imputeIplotdataout()$pointrugdf
        ppx<-ggplot(pointrugdf, aes(Imputation_Values, Original_Values,col=Groups)) +
          ggrastr::geom_point_rast() +
          scale_color_manual(values = c("#DA70D6","#7B68EE"))+
          theme_bw()+
          theme(legend.position = "bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
        ppx2<-ggMarginal(ppx, groupColour = TRUE, groupFill = TRUE, xparams = list(bins = 30,position ="dodge"),
                         yparams = list(bins = 30,position ="dodge"), type = "histogram")
        plot_grid(ppx1,ppx2,nrow=1,ncol=2)
      })
      output$imputeIplotdl<-downloadHandler(
        filename = function(){paste("Impute_I_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+2,height = pinjiafigheightx()/100)
          print(imputeIplotout())
          dev.off()
        }
      )
      #
      output$nrmseres<-renderDataTable({
        datatable(nrmsedfout(), options = list(pageLength = 20))
      })
      output$nrmseresdl<-downloadHandler(
        filename = function(){paste("NRMSE_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(nrmsedfout(),file,row.names = F)
        }
      )
      output$sornrmseres<-renderDataTable({
        datatable(sornrmsedfout(), options = list(pageLength = 20))
      })
      output$sornrmseresdl<-downloadHandler(
        filename = function(){paste("SORNRMSE_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(sornrmsedfout(),file,row.names = F)
        }
      )
      output$avgcor<-renderDataTable({
        datatable(avgcorout(), options = list(pageLength = 20))
      })
      output$avgcordl<-downloadHandler(
        filename = function(){paste("AvgCor_OI_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(avgcorout(),file,row.names = F)
        }
      )
      output$pssres<-renderDataTable({
        datatable(pssresout(), options = list(pageLength = 20))
      })
      output$pssresdl<-downloadHandler(
        filename = function(){paste("PSS_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(pssresout(),file,row.names = F)
        }
      )
      output$assessplot<-renderPlot({
        nrmsedf<-nrmsedfout()
        nrmsedf$Groups<-"NRMS"
        colnames(nrmsedf)<-c("Methods","Values","Groups")
        sornrmsedf<-sornrmsedfout()
        sornrmsedf$Groups<-"SOR"
        colnames(sornrmsedf)<-c("Methods","Values","Groups")
        avgcor<-avgcorout()
        avgcor$Groups<-"ACC_OI"#1/ACC_OI
        colnames(avgcor)<-c("Methods","Values","Groups")
        avgcor$Values<-1/avgcor$Values
        pssres<-pssresout()
        pssres$Groups<-"PSS"
        colnames(pssres)<-c("Methods","Values","Groups")
        assessplotdf<-rbind(nrmsedf,sornrmsedf,avgcor,pssres)
        pdx <- assessplotdf %>%group_by(Groups) %>%ungroup() %>%arrange(Groups, Values) %>%mutate(order = row_number())
        ggplot(pdx, aes(order, Values, color=Groups)) +
          geom_point(aes(shape=Groups), size=5) +
          geom_line(aes(group=Groups),size=1.4) +
          scale_x_continuous(breaks = pdx$order,labels = pdx$Methods,expand = c(0.02,0.02)) +
          facet_wrap(~ Groups, scales = "free") +
          xlab("Methods") +
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      assessplotout<-reactive({
        nrmsedf<-nrmsedfout()
        nrmsedf$Groups<-"NRMS"
        colnames(nrmsedf)<-c("Methods","Values","Groups")
        sornrmsedf<-sornrmsedfout()
        sornrmsedf$Groups<-"SOR"
        colnames(sornrmsedf)<-c("Methods","Values","Groups")
        avgcor<-avgcorout()
        avgcor$Groups<-"ACC_OI"
        colnames(avgcor)<-c("Methods","Values","Groups")
        avgcor$Values<-1/avgcor$Values
        pssres<-pssresout()
        pssres$Groups<-"PSS"
        colnames(pssres)<-c("Methods","Values","Groups")
        assessplotdf<-rbind(nrmsedf,sornrmsedf,avgcor,pssres)
        pdx <- assessplotdf %>%group_by(Groups) %>%ungroup() %>%arrange(Groups, Values) %>%mutate(order = row_number())
        ggplot(pdx, aes(order, Values, color=Groups)) +
          geom_point(aes(shape=Groups), size=5) +
          geom_line(aes(group=Groups),size=1.4) +
          scale_x_continuous(breaks = pdx$order,labels = pdx$Methods,expand = c(0.02,0.02)) +
          facet_wrap(~ Groups, scales = "free") +
          xlab("Methods") +
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$assessplotdl<-downloadHandler(
        filename = function(){paste("AssessPlot",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1.5,height = pinjiafigheightx()/100+1)
          print(assessplotout())
          dev.off()
        }
      )
      output$assessrank<-renderDataTable({
        datatable(assessrankout(), options = list(pageLength = 20))
      })
      output$assessrankdl<-downloadHandler(
        filename = function(){paste("Classic_RankMean_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(assessrankout(),file)#,row.names = F
        }
      )
    }
  )
  output$imputaionIIresui<-renderUI({
    namethodsoutx<-tolower(na.omit(namethodsout()))
    selectInput("topnmethodindex","",choices = namethodsoutx)
  })
  imputaionyuanshiout<-reactive({
    yuanshidf<-cvfilterdataout()
    namethodsoutx<-tolower(na.omit(namethodsout()))
    namethodsoutx1<-c(namethodsoutx,"Finish")
    withProgress(message = 'Imputing data with ', style = "notification", detail = paste0("1 ",namethodsoutx[1]," processing..."), value = 0,{
      originaldataimputelist<-list()
      for(i in 1:length(namethodsoutx)){
        originaldataimputelist[[i]]<-nafunctions(yuanshidf,method=namethodsoutx[i])
        incProgress(1/length(namethodsoutx), detail = paste0(i," ",namethodsoutx[i]," Done ~",
                                                             namethodsoutx1[i+1]," processing..."))
      }
    })
    names(originaldataimputelist)<-tolower(namethodsoutx)
    originaldataimputelist
  })
  imputaionIIdataout<-reactive({
    yuanshiimputlist<-imputaionyuanshiout()
    yuanshiimputlist1<-yuanshiimputlist[[input$topnmethodindex]]
    yuanshiimputlist1
  })
  observeEvent(
    input$mcsbtn_imputationII,{
      namethodsoutx<-tolower(na.omit(namethodsout()))
      showModal(modalDialog(
        title = "Selected Methods",
        div("Dear user, you have chosen several methods as below:",style = "font-size:120%;"),br(),
        div(HTML(paste(paste0("&nbsp;&nbsp;&nbsp&nbspMethod ",1:length(namethodsoutx)),namethodsoutx,sep = ": ",collapse = "</br>"))),br(),
        div("Then click 'OK' and move on...",style = "font-size:120%;"),br(),
        size ="l",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      output$imputaionIIdata<-renderDataTable({
        datatable(imputaionIIdataout(), options = list(pageLength = 20))
      })
      output$imputaionIIdatadl<-downloadHandler(
        filename = function(){paste("Original_Result_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(imputaionIIdataout(),file)#,row.names = F
        }
      )
    }
  )
  proassessout<-reactive({
    datanonaoutx<-datanonaout()
    namethodsoutx<-tolower(na.omit(namethodsout()))
    suijinadatadfx<-suijidataout()$suijinadatadf
    suijidatajieguo<-suijidataimputeout()

    pepchargefunc<-function(datalist,nadata,rownamesx,namethods){
      accpcresoutx<-vector()
      accpclist<-accpclist_before<-list()
      withProgress(message = 'Methods for ACC_Charges ', style = "notification", detail = "processing...", value = 0,{
        for(ii in 1:length(namethods)){
          dataii<-datalist[[namethods[ii]]]
          phosdata_nona<-cbind.data.frame(EG.PrecursorId=rownamesx,dataii,stringsAsFactors=FALSE)
          pepseqs_nona<-strsplit(phosdata_nona$EG.PrecursorId,"_")
          pepseqsm_nona<-as.data.frame(do.call(rbind,pepseqs_nona),stringsAsFactors=F)
          pep_nona_table<-sort(table(pepseqsm_nona$V1),decreasing = TRUE)#
          cornona_names<-dimnames(pep_nona_table)[[1]][as.numeric(pep_nona_table)>1]
          corval_nona<-corval_youna<-vector()
          withProgress(message = 'Calculating each object ', style = "notification", detail = "processing...",
                       value = 0,max = length(cornona_names),{
                         for(i in 1:length(cornona_names)){
                           index1<-grep(cornona_names[i],rownamesx,fixed = TRUE)#phosdata_nona$EG.PrecursorId
                           datai<-phosdata_nona[index1,-1]
                           corvali1<-cor(t(datai))
                           corvali2<-corvali1[upper.tri(corvali1)]
                           corval_nona[i]<-mean(corvali2)
                           dataix<-nadata[index1,]
                           corvali1x<-cor(t(dataix),use = "pairwise.complete.obs")
                           corvali2x<-corvali1x[upper.tri(corvali1x)]
                           corval_youna[i]<-mean(corvali2x)
                           incProgress(1, detail = "processing...")
                         }
                       })
          accpcresoutx[ii]<-mean(corval_nona)#sum(corval_nona)
          accpclist[[ii]]<-data.frame(Cor_charges=corval_nona)
          accpclist_before[[ii]]<-data.frame(Cor_charges_before=corval_youna)

          incProgress(1/length(namethods), detail = paste(namethods[ii],"processing..."))
        }
      })

      names(accpclist)<-namethods
      names(accpclist_before)<-namethods
      accpcresdf<-data.frame(Methods=namethods,ACC_Charge=round(accpcresoutx,digits=5),stringsAsFactors = FALSE)
      accpcresdf<-accpcresdf[order(accpcresdf$ACC_Charge,decreasing = TRUE),]
      rownames(accpcresdf)<-paste0("Method ",rownames(accpcresdf))
      list(accpcresdf=accpcresdf,accpclist_before=accpclist_before,accpclist=accpclist)
    }
    pepprofunc<-function(datalist,nadata,rownamesx,namethods){
      accpcresoutx<-vector()
      accpclist<-accpclist_before<-list()
      withProgress(message = 'Methods for ACC_PepProt ', style = "notification", detail = "processing...", value = 0,{
        for(ii in 1:length(namethods)){
          dataii<-datalist[[namethods[ii]]]
          phosdata_nona<-cbind.data.frame(EG.PrecursorId=rownamesx,dataii,stringsAsFactors=FALSE)
          pepseqs_nona<-strsplit(phosdata_nona$EG.PrecursorId,"_")
          pepseqsm_nona<-as.data.frame(do.call(rbind,pepseqs_nona),stringsAsFactors=F)
          pep_nona_table<-sort(table(pepseqsm_nona$V2),decreasing = TRUE)
          cornona_names<-dimnames(pep_nona_table)[[1]][as.numeric(pep_nona_table)>1]
          corval_nona<-corval_youna<-vector()
          withProgress(message = 'Calculating each object ', style = "notification", detail = "processing...",
                       value = 0,max = length(cornona_names),{
                         for(i in 1:length(cornona_names)){
                           index1<-grep(cornona_names[i],rownamesx,fixed = TRUE)#phosdata_nona$EG.PrecursorId
                           datai<-phosdata_nona[index1,-1]
                           corvali1<-cor(t(datai))
                           corvali2<-corvali1[upper.tri(corvali1)]
                           corval_nona[i]<-mean(corvali2)
                           dataix<-nadata[index1,]
                           corvali1x<-cor(t(dataix),use = "pairwise.complete.obs")
                           corvali2x<-corvali1x[upper.tri(corvali1x)]
                           corval_youna[i]<-mean(corvali2x)
                           incProgress(1, detail = "processing...")
                         }
                       })

          accpcresoutx[ii]<-mean(corval_nona)#sum(corval_nona)
          accpclist[[ii]]<-data.frame(Cor_peppro=corval_nona)
          accpclist_before[[ii]]<-data.frame(Cor_peppro_before=corval_youna)

          incProgress(1/length(namethods), detail = paste(namethods[ii],"processing..."))
        }
      })

      names(accpclist)<-namethods
      names(accpclist_before)<-namethods
      accpcresdf<-data.frame(Methods=namethods,ACC_peppro=round(accpcresoutx,digits=5),stringsAsFactors = FALSE)
      accpcresdf<-accpcresdf[order(accpcresdf$ACC_peppro,decreasing = TRUE),]
      rownames(accpcresdf)<-paste0("Method ",rownames(accpcresdf))
      list(pepproresdf=accpcresdf,pepprolist_before=accpclist_before,pepprolist=accpclist)
    }
    procomplexfunc<-function(datalist,nadata,rownamesx,namethods,complexdata){
      accpcresoutx<-vector()
      accpclist<-accpclist_before<-list()
      withProgress(message = 'Methods for ACC_CORUM ', style = "notification", detail = "processing...", value = 0,{
        for(ii in 1:length(namethods)){
          dataii<-datalist[[namethods[ii]]]
          phosdata_nona<-cbind.data.frame(EG.PrecursorId=rownamesx,dataii,stringsAsFactors=FALSE)
          pepseqs_nona<-strsplit(phosdata_nona$EG.PrecursorId,"_")
          pepseqsm_nona<-as.data.frame(do.call(rbind,pepseqs_nona),stringsAsFactors=F)
          colnames(pepseqsm_nona)<-c("Peptides","Names")
          pro_melt<-base::merge(pepseqsm_nona,complexdata,by = "Names",sort=FALSE)
          pep_nona_table<-sort(table(pro_melt$corum_id),decreasing = TRUE)
          cornona_names<-as.numeric(dimnames(pep_nona_table)[[1]][as.numeric(pep_nona_table)>1])
          corval_nona<-corval_youna<-vector()
          withProgress(message = 'Calculating each object ', style = "notification", detail = "processing...",
                       value = 0,max = length(cornona_names),{
                         for(i in 1:length(cornona_names)){
                           index1<-which(pro_melt$corum_id==cornona_names[i])
                           proindex1<-pro_melt[index1,]
                           datai<-dataii[pepseqsm_nona$Names%in%unique(proindex1$Names),]
                           corvali1<-cor(t(datai))
                           corvali2<-corvali1[upper.tri(corvali1)]
                           corval_nona[i]<-mean(corvali2)
                           dataix<-nadata[pepseqsm_nona$Names%in%unique(proindex1$Names),]
                           corvali1x<-cor(t(dataix),use = "pairwise.complete.obs")
                           corvali2x<-corvali1x[upper.tri(corvali1x)]
                           corval_youna[i]<-mean(corvali2x)
                           incProgress(1, detail = "processing...")
                         }
                       })

          accpcresoutx[ii]<-mean(corval_nona)#sum(corval_nona)
          accpclist[[ii]]<-data.frame(Cor_procomplex=corval_nona)
          accpclist_before[[ii]]<-data.frame(Cor_procomplex_before=corval_youna)
          incProgress(1/length(namethods), detail = paste(namethods[ii],"processing..."))
        }
      })
      names(accpclist)<-namethods
      names(accpclist_before)<-namethods
      accpcresdf<-data.frame(Methods=namethods,ACC_CORUM=round(accpcresoutx,digits=5),stringsAsFactors = FALSE)
      accpcresdf<-accpcresdf[order(accpcresdf$ACC_CORUM,decreasing = TRUE),]
      rownames(accpcresdf)<-paste0("Method ",rownames(accpcresdf))
      list(procomplexresdf=accpcresdf,procomplexlist_before=accpclist_before,procomplexlist=accpclist)
    }
    prohumapfunc<-function(datalist,nadata,rownamesx,namethods,complexdata){
      accpcresoutx<-vector()
      accpclist<-accpclist_before<-list()
      withProgress(message = 'Methods for ACC_PPI ', style = "notification", detail = "processing...", value = 0,{
        for(ii in 1:length(namethods)){
          dataii<-datalist[[namethods[ii]]]
          phosdata_nona<-cbind.data.frame(EG.PrecursorId=rownamesx,dataii,stringsAsFactors=FALSE)
          pepseqs_nona<-strsplit(phosdata_nona$EG.PrecursorId,"_")
          pepseqsm_nona<-as.data.frame(do.call(rbind,pepseqs_nona),stringsAsFactors=F)
          colnames(pepseqsm_nona)<-c("Peptides","Names")
          pro_melt<-base::merge(pepseqsm_nona,complexdata,by = "Names",sort=FALSE)
          pep_nona_table<-sort(table(pro_melt$corum_id),decreasing = TRUE)
          cornona_names<-as.numeric(dimnames(pep_nona_table)[[1]][as.numeric(pep_nona_table)>1])
          #dataiixx<-dataii[pepseqsm_nona$Names%in%pro_melt$Names,]
          corval_nona<-corval_youna<-vector()
          withProgress(message = 'Calculating each object ', style = "notification", detail = "processing...", value = 0,{
            for(i in 1:length(cornona_names)){
              index1<-which(pro_melt$corum_id==cornona_names[i])
              proindex1<-pro_melt[index1,]
              datai<-dataii[pepseqsm_nona$Names%in%unique(proindex1$Names),]
              corvali1<-cor(t(datai))
              corvali2<-corvali1[upper.tri(corvali1)]
              corval_nona[i]<-mean(corvali2)
              dataix<-nadata[pepseqsm_nona$Names%in%unique(proindex1$Names),]
              corvali1x<-cor(t(dataix),use = "pairwise.complete.obs")
              corvali2x<-corvali1x[upper.tri(corvali1x)]
              corval_youna[i]<-mean(corvali2x)
              incProgress(1/length(cornona_names), detail = "processing...")
            }
          })

          accpcresoutx[ii]<-mean(corval_nona)#sum(corval_nona)
          accpclist[[ii]]<-data.frame(Cor_procomplex=corval_nona)
          accpclist_before[[ii]]<-data.frame(Cor_procomplex_before=corval_youna)
          incProgress(1/length(namethods), detail = paste(namethods[ii],"processing..."))
        }
      })

      names(accpclist)<-namethods
      names(accpclist_before)<-namethods
      accpcresdf<-data.frame(Methods=namethods,ACC_PPI=round(accpcresoutx,digits=5),stringsAsFactors = FALSE)
      accpcresdf<-accpcresdf[order(accpcresdf$ACC_PPI,decreasing = TRUE),]
      rownames(accpcresdf)<-paste0("Method ",rownames(accpcresdf))
      list(prohumapresdf=accpcresdf,prohumaplist_before=accpclist_before,prohumaplist=accpclist)
    }


    if(input$datatypex==1){
      proassesslist<-pepchargefunc(datalist=suijidatajieguo,nadata=suijinadatadfx,
                                   rownamesx=rownames(suijinadatadfx),namethods=namethodsoutx)
    }
    if(input$datatypex==2){
      complexdata<-read.csv("uniprot_corum_mapping.txt",sep = "\t",stringsAsFactors = FALSE)
      colnames(complexdata)<-c("Names","corum_id")
      proassesslist1<-pepprofunc(datalist=suijidatajieguo,nadata=suijinadatadfx,
                                 rownamesx=rownames(suijinadatadfx),namethods=namethodsoutx)
      proassesslist2<-procomplexfunc(datalist=suijidatajieguo,nadata=suijinadatadfx,rownamesx=rownames(suijinadatadfx),
                                     namethods=namethodsoutx,complexdata=complexdata)
      humapdata<-read.csv("uniprotclust191015.csv",stringsAsFactors = FALSE)
      colnames(humapdata)<-c("Names","corum_id")
      proassesslist3<-prohumapfunc(datalist=suijidatajieguo,nadata=suijinadatadfx,rownamesx=rownames(suijinadatadfx),
                                   namethods=namethodsoutx,complexdata=humapdata)
      proassesslist<-c(proassesslist1,proassesslist2,proassesslist3)
    }
    if(input$datatypex==3){
      complexdata<-read.csv("uniprot_corum_mapping.txt",sep = "\t",stringsAsFactors = FALSE)
      colnames(complexdata)<-c("Names","corum_id")
      rownamesx<-as.data.frame(do.call(rbind,strsplit(rownames(suijinadatadfx),"_")))
      rownamesx1<-paste0(rownamesx[[1]],"_",rownamesx[[2]])
      proassesslist1<-pepchargefunc(datalist=suijidatajieguo,nadata=suijinadatadfx,
                                    rownamesx=rownamesx1,namethods=namethodsoutx)
      rownamesx2<-paste0(rownamesx[[1]],"_",rownamesx[[3]])
      proassesslist2<-pepprofunc(datalist=suijidatajieguo,nadata=suijinadatadfx,
                                 rownamesx=rownamesx2,namethods=namethodsoutx)
      proassesslist3<-procomplexfunc(datalist=suijidatajieguo,nadata=suijinadatadfx,rownamesx=rownamesx2,
                                     namethods=namethodsoutx,complexdata=complexdata)
      humapdata<-read.csv("uniprotclust191015.csv",stringsAsFactors = FALSE)
      colnames(humapdata)<-c("Names","corum_id")
      proassesslist4<-prohumapfunc(datalist=suijidatajieguo,nadata=suijinadatadfx,rownamesx=rownamesx2,
                                   namethods=namethodsoutx,complexdata=humapdata)
      proassesslist<-c(proassesslist1,proassesslist2,proassesslist3,proassesslist4)
    }
    if(input$datatypex==4){
      complexdata<-read.csv("uniprot_corum_mapping.txt",sep = "\t",stringsAsFactors = FALSE)
      colnames(complexdata)<-c("Names","corum_id")
      proassesslist1<-procomplexfunc(datalist=suijidatajieguo,nadata=suijinadatadfx,rownamesx=paste0("PepX_",rownames(suijinadatadfx)),
                                     namethods=namethodsoutx,complexdata=complexdata)
      humapdata<-read.csv("uniprotclust191015.csv",stringsAsFactors = FALSE)
      colnames(humapdata)<-c("Names","corum_id")
      proassesslist4<-prohumapfunc(datalist=suijidatajieguo,nadata=suijinadatadfx,rownamesx=paste0("PepX_",rownames(suijinadatadfx)),
                                   namethods=namethodsoutx,complexdata=humapdata)
      proassesslist<-c(proassesslist1,proassesslist4)
    }
    incProgress(1/2, detail = "processing...")
    proassesslist
  })
  ####2####
  accpprank2out<-reactive({
    accppres2df<-proassessout()$pepproresdf
    accppres2df$PepProt_Rank<-rank(1/proassessout()$pepproresdf$ACC_peppro)
    acccomplexres2df<-proassessout()$procomplexresdf
    acccomplexres2df$CORUM_Rank<-rank(1/proassessout()$procomplexresdf$ACC_CORUM)
    acchumapres2df<-proassessout()$prohumapresdf
    acchumapres2df$PPI_Rank<-rank(1/proassessout()$prohumapresdf$ACC_PPI)
    rankdf<-Reduce(function(x, y) merge(x, y, all=TRUE), list(accppres2df, acccomplexres2df, acchumapres2df))
    rankdf1<-rankdf[,c(1,3,5,7)]
    #
    if(input$customproteomiccriteriaif){
      xuanzecc<-paste0(input$proteomiccriteriaxuabze,"_Rank")
      xuanzeccweight<-as.numeric(strsplit(input$proteomiccriteriaweight,";")[[1]])
      rankdf2<-as.matrix(rankdf1[,xuanzecc,drop=TRUE])
      rankdf1$Rank_Mean<-rankdf2%*%c(xuanzeccweight/sum(xuanzeccweight))
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }else{
      rankdf1$Rank_Mean<-round(rowMeans(rankdf[,c(3,5,7)]),3)
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }
    rankdf1[order(rankdf1$Rank_Mean),]
  })
  ####3###
  accpprank3out<-reactive({
    accpcres3df<-proassessout()$accpcresdf
    accpcres3df$Charge_Rank<-rank(1/proassessout()$accpcresdf$ACC_Charge)
    accppres3df<-proassessout()$pepproresdf
    accppres3df$PepProt_Rank<-rank(1/proassessout()$pepproresdf$ACC_peppro)
    acccomplexres3df<-proassessout()$procomplexresdf
    acccomplexres3df$CORUM_Rank<-rank(1/proassessout()$procomplexresdf$ACC_CORUM)
    acchumapres3df<-proassessout()$prohumapresdf
    acchumapres3df$PPI_Rank<-rank(1/proassessout()$prohumapresdf$ACC_PPI)
    rankdf<-Reduce(function(x, y) merge(x, y, all=TRUE), list(accpcres3df,accppres3df, acccomplexres3df, acchumapres3df))
    rankdf1<-rankdf[,c(1,3,5,7,9)]
    #
    if(input$customproteomiccriteriaif){
      xuanzecc<-paste0(input$proteomiccriteriaxuabze,"_Rank")
      xuanzeccweight<-as.numeric(strsplit(input$proteomiccriteriaweight,";")[[1]])
      rankdf2<-as.matrix(rankdf1[,xuanzecc,drop=TRUE])
      rankdf1$Rank_Mean<-rankdf2%*%c(xuanzeccweight/sum(xuanzeccweight))
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }else{
      rankdf1$Rank_Mean<-round(rowMeans(rankdf[,c(3,5,7,9)]),3)
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }
    rankdf1[order(rankdf1$Rank_Mean),]
  })
  #####4####
  accpprank4out<-reactive({
    acccomplexres4df<-proassessout()$procomplexresdf
    acccomplexres4df$CORUM_Rank<-rank(1/proassessout()$procomplexresdf$ACC_CORUM)
    acchumapres4df<-proassessout()$prohumapresdf
    acchumapres4df$PPI_Rank<-rank(1/proassessout()$prohumapresdf$ACC_PPI)
    rankdf<-Reduce(function(x, y) merge(x, y, all=TRUE), list(acccomplexres4df, acchumapres4df))
    rankdf1<-rankdf[,c(1,3,5)]
    #
    if(input$customproteomiccriteriaif){
      xuanzecc<-paste0(input$proteomiccriteriaxuabze,"_Rank")
      xuanzeccweight<-as.numeric(strsplit(input$proteomiccriteriaweight,";")[[1]])
      rankdf2<-as.matrix(rankdf1[,xuanzecc,drop=TRUE])
      rankdf1$Rank_Mean<-rankdf2%*%c(xuanzeccweight/sum(xuanzeccweight))
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }else{
      rankdf1$Rank_Mean<-round(rowMeans(rankdf[,c(3,5)]),3)
      rownames(rankdf1)<-paste0("Method ",rownames(rankdf1))
    }
    rankdf1[order(rankdf1$Rank_Mean),]
  })

  observeEvent(
    input$mcsbtn_imputationpro,{
      shinyjs::show(id = "mcsbtn_imputationpro_hid", anim = FALSE)
      output$accpcres1<-renderDataTable({
        datatable(proassessout()$accpcresdf, options = list(pageLength = 20))
      })
      output$accpcresdl1<-downloadHandler(
        filename = function(){paste("ACC_Charge_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$accpcresdf,file,row.names = F)
        }
      )
      output$histaccpcplot1<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
        accpchist_before<-proassessout()$accpclist_before[[namethodsx]]
        accpchist<-proassessout()$accpclist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histaccpcplotout1<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
        accpchist_before<-proassessout()$accpclist_before[[namethodsx]]
        accpchist<-proassessout()$accpclist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$histaccpcplotdl1<-downloadHandler(
        filename = function(){paste("PepCharges_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histaccpcplotout1())
          dev.off()
        }
      )
      ########2###
      output$accpprank2<-renderDataTable({
        datatable(accpprank2out(), options = list(pageLength = 20))
      })
      output$accpprankdl2<-downloadHandler(
        filename = function(){paste("Proteomic_Rankmean_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(accpprank2out(),file,row.names = F)
        }
      )
      #
      output$accppres2<-renderDataTable({
        datatable(proassessout()$pepproresdf, options = list(pageLength = 20))
      })
      output$accppresdl2<-downloadHandler(
        filename = function(){paste("ACC_PepProt_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$pepproresdf,file,row.names = F)
        }
      )
      output$histaccppplot2<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
        accpchist_before<-proassessout()$pepprolist_before[[namethodsx]]
        accpchist<-proassessout()$pepprolist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histaccppplotout2<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
        accpchist_before<-proassessout()$pepprolist_before[[namethodsx]]
        accpchist<-proassessout()$pepprolist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$histaccppplotdl2<-downloadHandler(
        filename = function(){paste("PepPro_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histaccppplotout2())
          dev.off()
        }
      )
      #
      output$acccomplexres2<-renderDataTable({
        datatable(proassessout()$procomplexresdf, options = list(pageLength = 20))
      })
      output$acccomplexresdl2<-downloadHandler(
        filename = function(){paste("ACC_CORUM_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$procomplexresdf,file,row.names = F)
        }
      )
      output$histacccomplexplot2<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
        accpchist_before<-proassessout()$procomplexlist_before[[namethodsx]]
        accpchist<-proassessout()$procomplexlist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histacccomplexplotout2<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex
        accpchist_before<-proassessout()$procomplexlist_before[[namethodsx]]
        accpchist<-proassessout()$procomplexlist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$histacccomplexplotdl2<-downloadHandler(
        filename = function(){paste("ProCORUM_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histacccomplexplotout2())
          dev.off()
        }
      )
      #
      output$acchumapres2<-renderDataTable({
        datatable(proassessout()$prohumapresdf, options = list(pageLength = 20))
      })
      output$acchumapresdl2<-downloadHandler(
        filename = function(){paste("ACC_PPI_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$prohumapresdf,file,row.names = F)
        }
      )
      output$histacchumapplot2<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$prohumaplist_before[[namethodsx]]
        accpchist<-proassessout()$prohumaplist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histacchumapplotout2<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$prohumaplist_before[[namethodsx]]
        accpchist<-proassessout()$prohumaplist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$histacchumapplotdl2<-downloadHandler(
        filename = function(){paste("ProPPI_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},##suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histacchumapplotout2())
          dev.off()
        }
      )
      ########3###
      output$accpprank3<-renderDataTable({
        datatable(accpprank3out(), options = list(pageLength = 20))
      })
      output$accpprankdl3<-downloadHandler(
        filename = function(){paste("Proteomic_Rankmean_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(accpprank3out(),file,row.names = F)
        }
      )
      #
      output$accpcres3<-renderDataTable({
        datatable(proassessout()$accpcresdf, options = list(pageLength = 20))
      })
      output$accpcresdl3<-downloadHandler(
        filename = function(){paste("ACC_Charge_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$accpcresdf,file,row.names = F)
        }
      )
      output$histaccpcplot3<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$accpclist_before[[namethodsx]]
        accpchist<-proassessout()$accpclist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histaccpcplotout3<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$accpclist_before[[namethodsx]]
        accpchist<-proassessout()$accpclist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14),
                plot.title = element_text(hjust = 1))
      })
      output$histaccpcplotdl3<-downloadHandler(
        filename = function(){paste("PepCharges_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histaccpcplotout3())
          dev.off()
        }
      )
      output$accppres3<-renderDataTable({
        datatable(proassessout()$pepproresdf, options = list(pageLength = 20))
      })
      output$accppresdl3<-downloadHandler(
        filename = function(){paste("ACC_PepProt_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$pepproresdf,file,row.names = F)
        }
      )
      output$histaccppplot3<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$pepprolist_before[[namethodsx]]
        accpchist<-proassessout()$pepprolist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histaccppplotout3<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$pepprolist_before[[namethodsx]]
        accpchist<-proassessout()$pepprolist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14),
                plot.title = element_text(hjust = 1))
      })
      output$histaccppplotdl3<-downloadHandler(
        filename = function(){paste("PepPro_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histaccppplotout3())
          dev.off()
        }
      )
      #
      output$acccomplexres3<-renderDataTable({
        datatable(proassessout()$procomplexresdf, options = list(pageLength = 20))
      })
      output$acccomplexresdl3<-downloadHandler(
        filename = function(){paste("ACC_CORUM_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$procomplexresdf,file,row.names = F)
        }
      )
      output$histacccomplexplot3<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$procomplexlist_before[[namethodsx]]
        accpchist<-proassessout()$procomplexlist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histacccomplexplotout3<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$procomplexlist_before[[namethodsx]]
        accpchist<-proassessout()$procomplexlist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14),
                plot.title = element_text(hjust = 1))
      })
      output$histacccomplexplotdl3<-downloadHandler(
        filename = function(){paste("ProCORUM_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histacccomplexplotout3())
          dev.off()
        }
      )
      #
      output$acchumapres3<-renderDataTable({
        datatable(proassessout()$prohumapresdf, options = list(pageLength = 20))
      })
      output$acchumapresdl3<-downloadHandler(
        filename = function(){paste("ACC_PPI_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$prohumapresdf,file,row.names = F)
        }
      )
      output$histacchumapplot3<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$prohumaplist_before[[namethodsx]]
        accpchist<-proassessout()$prohumaplist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histacchumapplotout3<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$prohumaplist_before[[namethodsx]]
        accpchist<-proassessout()$prohumaplist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14),
                plot.title = element_text(hjust = 1))
      })
      output$histacchumapplotdl3<-downloadHandler(
        filename = function(){paste("ProPPI_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histacchumapplotout3())
          dev.off()
        }
      )
      #
      output$accproallplot<-renderPlot({
        ppx1<-histaccpcplotout3()
        ppx2<-histaccppplotout3()
        ppx3<-histacccomplexplotout3()
        ppx4<-histacchumapplotout3()
        plot_grid(ppx1,ppx2,ppx3,ppx4,nrow=2,ncol=2,labels=c("ACC_Charges", "ACC_PepProt", "ACC_CORUM", "ACC_PPI"))
      },height = pinjiafigheightx)
      accproallplotout<-reactive({
        ppx1<-histaccpcplotout3()
        ppx2<-histaccppplotout3()
        ppx3<-histacccomplexplotout3()
        ppx4<-histacchumapplotout3()
        plot_grid(ppx1,ppx2,ppx3,ppx4,nrow=2,ncol=2,labels=c("ACC_Charges", "ACC_PepProt", "ACC_CORUM", "ACC_PPI"))
      })
      output$accproallplotdl<-downloadHandler(
        filename = function(){paste("Proteomic_Criteria_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+3,height = pinjiafigheightx()/100+3)
          print(accproallplotout())
          dev.off()
        }
      )
      ########4##
      output$accpprank4<-renderDataTable({
        datatable(accpprank4out(), options = list(pageLength = 20))
      })
      output$accpprankdl4<-downloadHandler(
        filename = function(){paste("Proteomic_Rankmean_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(accpprank4out(),file,row.names = F)
        }
      )
      #
      output$acccomplexres4<-renderDataTable({
        datatable(proassessout()$procomplexresdf, options = list(pageLength = 20))
      })
      output$acccomplexresdl4<-downloadHandler(
        filename = function(){paste("ACC_CORUM_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$procomplexresdf,file,row.names = F)
        }
      )
      output$histacccomplexplot4<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$procomplexlist_before[[namethodsx]]
        accpchist<-proassessout()$procomplexlist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histacccomplexplotout4<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$procomplexlist_before[[namethodsx]]
        accpchist<-proassessout()$procomplexlist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$histacccomplexplotdl4<-downloadHandler(
        filename = function(){paste("ProCORUM_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histacccomplexplotout4())
          dev.off()
        }
      )
      #
      output$acchumapres4<-renderDataTable({
        datatable(proassessout()$prohumapresdf, options = list(pageLength = 20))
      })
      output$acchumapresdl4<-downloadHandler(
        filename = function(){paste("ACC_PPI_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(proassessout()$prohumapresdf,file,row.names = F)
        }
      )
      output$histacchumapplot4<-renderPlot({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$prohumaplist_before[[namethodsx]]
        accpchist<-proassessout()$prohumaplist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = pinjiafigheightx)
      histacchumapplotout4<-reactive({
        namethodsx<-tolower(input$topnmethodindex)#suijijieguoindex)
        accpchist_before<-proassessout()$prohumaplist_before[[namethodsx]]
        accpchist<-proassessout()$prohumaplist[[namethodsx]]
        accpchistdf<-data.frame(Groups=rep(c("Before","After"),c(nrow(accpchist_before),nrow(accpchist))),
                                Values=c(accpchist_before[[1]],accpchist[[1]]),stringsAsFactors = FALSE)

        mudf <- plyr::ddply(accpchistdf, "Groups", summarise, grp.mean=median(Values))
        mudf$grp.mean<-round(mudf$grp.mean,3)
        ggplot(accpchistdf, aes(x=Values, color=Groups, fill=Groups)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
          geom_density(alpha=0.2,size=1)+
          geom_vline(data=mudf, aes(xintercept=grp.mean, color=Groups),
                     linetype="dashed",size=1)+
          scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                         paste0("Before (median: ",mudf$grp.mean[2],")")))+
          scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(paste0("After (median: ",mudf$grp.mean[1],")"),
                                                                        paste0("Before (median: ",mudf$grp.mean[2],")")))+
          labs(x="Correlation",y="Density",title = paste0("Method: ",namethodsx))+
          theme_bw()+
          theme(legend.position="bottom",axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$histacchumapplotdl4<-downloadHandler(
        filename = function(){paste("ProPPI_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},#suijijieguoindex
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+1,height = pinjiafigheightx()/100)
          print(histacchumapplotout4())
          dev.off()
        }
      )
    }
  )
  ##
  output$finalcheck1<-renderUI({
    nrmsedf<-nrmsedfout()
    sornrmsedf<-sornrmsedfout()
    avgcor<-avgcorout()
    pssres<-pssresout()
    assessrankdf<-assessrankout()
    if(input$datatypex==1){
      procriteria1<-proassessout()$accpcresdf
      div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:130%;color:black",
          HTML("~~ <em>Check information for final imputation results</em> ~~<br />"),
          HTML(paste0("&nbsp;&nbsp;1. Based on the classic criteria, The rank first method is ",strong(assessrankdf$Methods[1]),
                      " (NRMSE: ",nrmsedf[[2]][nrmsedf[[1]]==assessrankdf$Methods[1]],", SOR: ",sornrmsedf[[2]][sornrmsedf[[1]]==assessrankdf$Methods[1]],
                      ", ACC_OI: ",avgcor[[2]][avgcor[[1]]==assessrankdf$Methods[1]],", PSS: ",pssres[[2]][pssres[[1]]==assessrankdf$Methods[1]],
                      ");<br />")),
          HTML(paste0("&nbsp;&nbsp;2. Based on the proteomic criteria, The rank first method is ",strong(pjalldf$Methods[1]),
                      " (ACC_Charge: ",procriteria1[[2]][procriteria1[[1]]==pjalldf$Methods[1]],
                      ").<br />")))
    }else if(input$datatypex==2){
      procriteria2<-proassessout()$pepproresdf
      procriteria3<-proassessout()$procomplexresdf
      procriteria4<-proassessout()$prohumapresdf
      pjalldf<-accpprank2out()
      div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:130%;color:black",
          HTML("~~ <em>Check information for final imputation results</em> ~~<br />"),
          HTML(paste0("&nbsp;&nbsp;1. Based on the classic criteria, The rank first method is ",strong(assessrankdf$Methods[1]),
                      " (NRMSE: ",nrmsedf[[2]][nrmsedf[[1]]==assessrankdf$Methods[1]],", SOR: ",sornrmsedf[[2]][sornrmsedf[[1]]==assessrankdf$Methods[1]],
                      ", ACC_OI: ",avgcor[[2]][avgcor[[1]]==assessrankdf$Methods[1]],", PSS: ",pssres[[2]][pssres[[1]]==assessrankdf$Methods[1]],
                      ");<br />")),
          HTML(paste0("&nbsp;&nbsp;2. Based on the proteomic criteria, The rank first method is ",strong(pjalldf$Methods[1]),
                      " (ACC_PepProt: ",
                      procriteria2[[2]][procriteria2[[1]]==pjalldf$Methods[1]],", ACC_CORUM: ",
                      procriteria3[[2]][procriteria3[[1]]==pjalldf$Methods[1]],", ACC_PPI: ",
                      procriteria4[[2]][procriteria4[[1]]==pjalldf$Methods[1]],
                      ").<br />")))
    }else if(input$datatypex==3){
      procriteria1<-proassessout()$accpcresdf
      procriteria2<-proassessout()$pepproresdf
      procriteria3<-proassessout()$procomplexresdf
      procriteria4<-proassessout()$prohumapresdf
      pjalldf<-accpprank3out()
      div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:130%;color:black",
          HTML("~~ <em>Check information for final imputation results</em> ~~<br />"),
          HTML(paste0("&nbsp;&nbsp;1. Based on the classic criteria, The rank first method is ",strong(assessrankdf$Methods[1]),
                      " (NRMSE: ",nrmsedf[[2]][nrmsedf[[1]]==assessrankdf$Methods[1]],", SOR: ",sornrmsedf[[2]][sornrmsedf[[1]]==assessrankdf$Methods[1]],
                      ", ACC_OI: ",avgcor[[2]][avgcor[[1]]==assessrankdf$Methods[1]],", PSS: ",pssres[[2]][pssres[[1]]==assessrankdf$Methods[1]],
                      ");<br />")),
          HTML(paste0("&nbsp;&nbsp;2. Based on the proteomic criteria, The rank first method is ",strong(pjalldf$Methods[1]),
                      " (ACC_Charge: ",procriteria1[[2]][procriteria1[[1]]==pjalldf$Methods[1]],", ACC_PepProt: ",
                      procriteria2[[2]][procriteria2[[1]]==pjalldf$Methods[1]],", ACC_CORUM: ",
                      procriteria3[[2]][procriteria3[[1]]==pjalldf$Methods[1]],", ACC_PPI: ",
                      procriteria4[[2]][procriteria4[[1]]==pjalldf$Methods[1]],
                      ").<br />")))
    }else if(input$datatypex==4){
      procriteria3<-proassessout()$procomplexresdf
      procriteria4<-proassessout()$prohumapresdf
      pjalldf<-accpprank4out()
      div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:130%;color:black",
          HTML("~~ <em>Check information for final imputation results</em> ~~<br />"),
          HTML(paste0("&nbsp;&nbsp;1. Based on the classic criteria, The rank first method is ",strong(assessrankdf$Methods[1]),
                      " (NRMSE: ",nrmsedf[[2]][nrmsedf[[1]]==assessrankdf$Methods[1]],", SOR: ",sornrmsedf[[2]][sornrmsedf[[1]]==assessrankdf$Methods[1]],
                      ", ACC_OI: ",avgcor[[2]][avgcor[[1]]==assessrankdf$Methods[1]],", PSS: ",pssres[[2]][pssres[[1]]==assessrankdf$Methods[1]],
                      ");<br />")),
          HTML(paste0("&nbsp;&nbsp;2. Based on the proteomic criteria, The rank first method is ",strong(pjalldf$Methods[1]),
                      " (ACC_CORUM: ",
                      procriteria3[[2]][procriteria3[[1]]==pjalldf$Methods[1]],", ACC_PPI: ",
                      procriteria4[[2]][procriteria4[[1]]==pjalldf$Methods[1]],
                      ").<br />")))
    }else{
      div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:130%;color:black",
          HTML("~~ <em>Check information for final imputation results</em> ~~<br />"),
          HTML(paste0("&nbsp;&nbsp;1. Based on the classic criteria, The rank first method is ",strong(assessrankdf$Methods[1]),
                      " (NRMSE: ",nrmsedf[[2]][nrmsedf[[1]]==assessrankdf$Methods[1]],", SOR: ",sornrmsedf[[2]][sornrmsedf[[1]]==assessrankdf$Methods[1]],
                      ", ACC_OI: ",avgcor[[2]][avgcor[[1]]==assessrankdf$Methods[1]],", PSS: ",pssres[[2]][pssres[[1]]==assessrankdf$Methods[1]],
                      ");<br />")))
    }
  })
  output$finalcheck2<-renderUI({
    nrmsedf<-nrmsedfout()
    sornrmsedf<-sornrmsedfout()
    avgcor<-avgcorout()
    pssres<-pssresout()
    classiccriteriafc<-c(max(nrmsedf[[2]])/min(nrmsedf[[2]]),max(sornrmsedf[[2]])/min(sornrmsedf[[2]]),
                         max(avgcor[[2]])/min(avgcor[[2]]),max(pssres[[2]])/min(pssres[[2]]))
    if(input$datatypex==1){
      procriteria1<-proassessout()$accpcresdf
      allcriteriafc<-c(classiccriteriafc,max(procriteria1[[2]])/min(procriteria1[[2]]))
      allcriteriafcnames<-c("NRMSE","SOR","ACC_OI","PSS","ACC_Charge")
      if(sum(allcriteriafc>=2)>=length(allcriteriafc)/2){
        div(style="text-align:left;margin-top:10px;margin-right:150px;font-size:150%;color:green",
            HTML(paste0("Finally, NAguideR thinks the imputation results are acceptable, you can choose the best one based on the classic criteria or proteomic criteria for future analysis.")))
      }else{
        div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:150%;color:red",
            HTML(paste0("Warning: NAguideR detects the scores based on more than half of the criteria don't seem to change much (",paste(allcriteriafcnames[which(allcriteriafc<2)],collapse = ", "),
                        "). The imputation results may not be quite acceptable, please check the results again.<br />")),
            hr(),
            HTML("Possible solutions:<br />"),
            HTML("&nbsp;&nbsp;1. Please check the input data quality in the step 2;<br />"),
            HTML("&nbsp;&nbsp;2. Please check the normalization and logarithm parameters, your data may need to be normalized and logarithmic, or vice verse;<br />"),
            HTML("&nbsp;&nbsp;3. The imputation methods you choose may be incompetent to deduce the proprt results, please choose more complex methods;<br />"),
            HTML("&nbsp;&nbsp;4. Please use targeted check for additional analysis. Even if no discriminative results were obtained, this does not mean the NA imputation method(s) failed. NAguideR just failed to provide a clear guidance."))
      }
    }else if(input$datatypex==2){
      procriteria2<-proassessout()$pepproresdf
      procriteria3<-proassessout()$procomplexresdf
      procriteria4<-proassessout()$prohumapresdf
      allcriteriafc<-c(classiccriteriafc,max(procriteria2[[2]])/min(procriteria2[[2]]),
                       max(procriteria3[[2]])/min(procriteria3[[2]]),max(procriteria4[[2]])/min(procriteria4[[2]]))
      allcriteriafcnames<-c("NRMSE","SOR","ACC_OI","PSS","ACC_PepProt","ACC_CORUM","ACC_PPI")
      if(sum(allcriteriafc>=2)>=length(allcriteriafc)/2){
        div(style="text-align:left;margin-top:10px;margin-right:150px;font-size:150%;color:green",
            HTML(paste0("Finally, NAguideR thinks the imputation results are acceptable, you can choose the best one based on the classic criteria or proteomic criteria for future analysis.")))
      }else{
        div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:150%;color:red",
            HTML(paste0("Warning: NAguideR detects the scores based on more than half of the criteria don't seem to change much (",paste(allcriteriafcnames[which(allcriteriafc<2)],collapse = ", "),
                        "). The imputation results may not be quite acceptable, please check the results again.<br />")),
            hr(),
            HTML("Possible solutions:<br />"),
            HTML("&nbsp;&nbsp;1. Please check the input data quality in the step 2;<br />"),
            HTML("&nbsp;&nbsp;2. Please check the normalization and logarithm parameters, your data may need to be normalized and logarithmic, or vice verse;<br />"),
            HTML("&nbsp;&nbsp;3. The imputation methods you choose may be incompetent to deduce the proprt results, please choose more complex methods;<br />"),
            HTML("&nbsp;&nbsp;4. Please use targeted check for additional analysis. Even if no discriminative results were obtained, this does not mean the NA imputation method(s) failed. NAguideR just failed to provide a clear guidance."))
      }
    }else if(input$datatypex==3){
      procriteria1<-proassessout()$accpcresdf
      procriteria2<-proassessout()$pepproresdf
      procriteria3<-proassessout()$procomplexresdf
      procriteria4<-proassessout()$prohumapresdf
      allcriteriafc<-c(classiccriteriafc,max(procriteria1[[2]])/min(procriteria1[[2]]),max(procriteria2[[2]])/min(procriteria2[[2]]),
                       max(procriteria3[[2]])/min(procriteria3[[2]]),max(procriteria4[[2]])/min(procriteria4[[2]]))
      allcriteriafcnames<-c("NRMSE","SOR","ACC_OI","PSS","ACC_Charge","ACC_PepProt","ACC_CORUM","ACC_PPI")
      if(sum(allcriteriafc>=2)>=length(allcriteriafc)/2){
        div(style="text-align:left;margin-top:10px;margin-right:150px;font-size:150%;color:green",
            HTML(paste0("Finally, NAguideR thinks the imputation results are acceptable, you can choose the best one based on the classic criteria or proteomic criteria for future analysis.")))
      }else{
        div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:150%;color:red",
            HTML(paste0("Warning: NAguideR detects the scores based on more than half of the criteria don't seem to change much (",paste(allcriteriafcnames[which(allcriteriafc<2)],collapse = ", "),
                        "). The imputation results may not be quite acceptable, please check the results again.<br />")),
            hr(),
            HTML("Possible solutions:<br />"),
            HTML("&nbsp;&nbsp;1. Please check the input data quality in the step 2;<br />"),
            HTML("&nbsp;&nbsp;2. Please check the normalization and logarithm parameters, your data may need to be normalized and logarithmic, or vice verse;<br />"),
            HTML("&nbsp;&nbsp;3. The imputation methods you choose may be incompetent to deduce the proprt results, please choose more complex methods;<br />"),
            HTML("&nbsp;&nbsp;4. Please use targeted check for additional analysis. Even if no discriminative results were obtained, this does not mean the NA imputation method(s) failed. NAguideR just failed to provide a clear guidance."))
      }
    }else if(input$datatypex==4){
      procriteria3<-proassessout()$procomplexresdf
      procriteria4<-proassessout()$prohumapresdf
      allcriteriafc<-c(classiccriteriafc,max(procriteria3[[2]])/min(procriteria3[[2]]),
                       max(procriteria4[[2]])/min(procriteria4[[2]]))
      allcriteriafcnames<-c("NRMSE","SOR","ACC_OI","PSS","ACC_CORUM","ACC_PPI")
      if(sum(allcriteriafc>=2)>=length(allcriteriafc)/2){
        div(style="text-align:left;margin-top:10px;margin-right:150px;font-size:150%;color:green",
            HTML(paste0("Finally, NAguideR thinks the imputation results are acceptable, you can choose the best one based on the classic criteria or proteomic criteria for future analysis.")))
      }else{
        div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:150%;color:red",
            HTML(paste0("Warning: NAguideR detects the scores based on more than half of the criteria don't seem to change much (",paste(allcriteriafcnames[which(allcriteriafc<2)],collapse = ", "),
                        "). The imputation results may not be quite acceptable, please check the results again.<br />")),
            hr(),
            HTML("Possible solutions:<br />"),
            HTML("&nbsp;&nbsp;1. Please check the input data quality in the step 2;<br />"),
            HTML("&nbsp;&nbsp;2. Please check the normalization and logarithm parameters, your data may need to be normalized and logarithmic, or vice verse;<br />"),
            HTML("&nbsp;&nbsp;3. The imputation methods you choose may be incompetent to deduce the proprt results, please choose more complex methods;<br />"),
            HTML("&nbsp;&nbsp;4. Please use targeted check for additional analysis. Even if no discriminative results were obtained, this does not mean the NA imputation method(s) failed. NAguideR just failed to provide a clear guidance."))
      }
    }else{
      allcriteriafc<-classiccriteriafc
      allcriteriafcnames<-c("NRMSE","SOR","ACC_OI","PSS")
      if(sum(allcriteriafc>=2)>=length(allcriteriafc)/2){
        div(style="text-align:left;margin-top:10px;margin-right:150px;font-size:150%;color:green",
            HTML(paste0("Finally, NAguideR thinks the imputation results are acceptable, you can choose the best one based on the classic criteria or proteomic criteria for future analysis.")))
      }else{
        div(style="text-align:left;margin-top:20px;margin-right:150px;font-size:150%;color:red",
            HTML(paste0("Warning: NAguideR detects the scores based on more than half of the criteria don't seem to change much (",paste(allcriteriafcnames[which(allcriteriafc<2)],collapse = ", "),
                        "). The imputation results may not be quite acceptable, please check the results again.<br />")),
            hr(),
            HTML("Possible solutions:<br />"),
            HTML("&nbsp;&nbsp;1. Please check the input data quality in the step 2;<br />"),
            HTML("&nbsp;&nbsp;2. Please check the normalization and logarithm parameters, your data may need to be normalized and logarithmic, or vice verse;<br />"),
            HTML("&nbsp;&nbsp;3. The imputation methods you choose may be incompetent to deduce the proprt results, please choose more complex methods;<br />"),
            HTML("&nbsp;&nbsp;4. Please use targeted check for additional analysis. Even if no discriminative results were obtained, this does not mean the NA imputation method(s) failed. NAguideR just failed to provide a clear guidance."))
      }
    }
  })

  observeEvent(
    input$mcsbtn_targettext,{
      shinyjs::show(id = "mcsbtn_targettext_hid", anim = FALSE)
      output$targettextplot<-renderPlot({
        targettextx1<-isolate(input$targettext)
        targettextx<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",gsub("\\]","\\\\]",gsub("\\[","\\\\[",targettextx1))))
        selectmethod<-input$topnmethodindex
        namethodsoutx<-tolower(na.omit(namethodsout()))
        yuanshidf<-cvfilterdataout()
        yuanshiimputlist<-imputaionyuanshiout()
        yuanshidf1<-yuanshidf[!complete.cases(yuanshidf),]
        whichpro1<-grep(targettextx,rownames(yuanshidf1),perl = TRUE)
        datanonaoutx<-datanonaout()
        suijinadatadfx<-suijidataout()$suijinadatadf
        suijidatajieguo<-suijidataimputeout()
        suijinadatadfx1<-suijinadatadfx[!complete.cases(suijinadatadfx),]
        whichpro2<-grep(targettextx,rownames(suijinadatadfx1),perl = TRUE)
        suijinadatadfx2<-suijinadatadfx[complete.cases(suijinadatadfx),]
        whichpro3<-grep(targettextx,rownames(suijinadatadfx2),perl = TRUE)
        #
        targetdfall<-NULL
        targetnanum1<-targetnanum2<-0
        if(length(whichpro1)>0){
          shuju1<-reshape2::melt(t(yuanshidf1[whichpro1,]))
          shuju1$Group<-"Original"
          whichpro1name<-rownames(yuanshidf1)[whichpro1]
          whichprox1df<-yuanshiimputlist[[selectmethod]]
          whichprox1df1<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
          shuju2<-reshape2::melt(t(whichprox1df1))
          shuju2$Group<-"Imputed"
          shuju3<-rbind(shuju1,shuju2)
          targetdfall<-rbind(targetdfall,shuju3)
          targetnanum1<-max(apply(yuanshidf1[whichpro1,],1,function(x){sum(is.na(x))}))
        }
        if(length(whichpro2)>0){
          whichpro1name<-rownames(suijinadatadfx1)[whichpro2]
          shuju1<-reshape2::melt(t(datanonaoutx[rownames(datanonaoutx)%in%whichpro1name,]))
          shuju1$Group<-"Original"
          whichprox1df<-suijidatajieguo[[selectmethod]]
          whichprox1df2<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
          shuju2<-reshape2::melt(t(whichprox1df2))
          shuju2$Group<-"Imputed"
          shuju3<-rbind(shuju1,shuju2)
          targetdfall<-rbind(targetdfall,shuju3)
          targetnanum2<-max(apply(suijinadatadfx1[whichpro2,],1,function(x){sum(is.na(x))}))
        }
        if(is.null(targetdfall)){
          if(length(whichpro3)>0){
            shuju1<-reshape2::melt(t(suijinadatadfx2[whichpro3,]))
            shuju1$Group<-"Original"
            whichpro1name<-rownames(suijinadatadfx2)[whichpro3]
            whichprox1df<-suijidatajieguo[[selectmethod]]
            whichprox1df2<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
            shuju2<-reshape2::melt(t(whichprox1df2))
            shuju2$Group<-"Imputed"
            shuju3<-rbind(shuju1,shuju2)
            targetdfall<-rbind(targetdfall,shuju3)
            targetdfall$Group<-factor(targetdfall$Group,levels = c("Original","Imputed"))
            ggplot(data=targetdfall, aes(x=Var1, y=value, fill=Group)) +
              geom_bar(stat="identity", color="black", position=position_dodge())+
              theme_bw()+
              facet_wrap(~Var2,ncol = 2)+
              theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.3),
                    plot.title = element_text(size=18),
                    plot.subtitle=element_text(size=15),
                    strip.text.x = element_text(size = 13))+
              labs(title="Target protein/peptide was not missed in any sample.",
                   subtitle=paste0("Imputation method: ",selectmethod),
                   x="Samples",y="Values")
          }else{
            plot(0,xlim=c(-1,1),ylim=c(-1,1), xaxt = "n", bty = "n", yaxt = "n", type = "n", xlab = "", ylab = "")
            text(0,0.5,labels = "Target protein/peptide not found. Please make sure the item is included in the input table!",cex=2)
          }
        }else{
          if(length(whichpro3)>0){
            shuju1<-reshape2::melt(t(suijinadatadfx2[whichpro3,]))
            shuju1$Group<-"Original"
            whichpro1name<-rownames(suijinadatadfx2)[whichpro3]
            whichprox1df<-suijidatajieguo[[selectmethod]]
            whichprox1df2<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
            shuju2<-reshape2::melt(t(whichprox1df2))
            shuju2$Group<-"Imputed"
            shuju3<-rbind(shuju1,shuju2)
            targetdfall<-rbind(targetdfall,shuju3)
          }
          targetdfall$Group<-factor(targetdfall$Group,levels = c("Original","Imputed"))
          ggplot(data=targetdfall, aes(x=Var1, y=value, fill=Group)) +
            geom_bar(stat="identity", color="black", position=position_dodge())+
            theme_bw()+
            facet_wrap(~Var2,ncol = 2)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.3),
                  plot.title = element_text(size=16),
                  plot.subtitle=element_text(size=14),
                  strip.text.x = element_text(size = 13))+
            labs(title=paste0("Target protein/peptide was missed in N=",max(c(targetnanum1,targetnanum2)),
                              " samples among all N=",ncol(yuanshidf)," samples"),
                 subtitle=paste0("Imputation method: ",selectmethod),
                 x="Samples",y="Values")
        }
      },height = pinjiafigheightx)
      targettextplotout<-reactive({
        targettextx1<-isolate(input$targettext)
        targettextx<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",gsub("\\]","\\\\]",gsub("\\[","\\\\[",targettextx1))))
        selectmethod<-input$topnmethodindex
        namethodsoutx<-tolower(na.omit(namethodsout()))
        yuanshidf<-cvfilterdataout()
        yuanshiimputlist<-imputaionyuanshiout()
        yuanshidf1<-yuanshidf[!complete.cases(yuanshidf),]
        whichpro1<-grep(targettextx,rownames(yuanshidf1),perl = TRUE)
        datanonaoutx<-datanonaout()
        suijinadatadfx<-suijidataout()$suijinadatadf
        suijidatajieguo<-suijidataimputeout()
        suijinadatadfx1<-suijinadatadfx[!complete.cases(suijinadatadfx),]
        whichpro2<-grep(targettextx,rownames(suijinadatadfx1),perl = TRUE)
        suijinadatadfx2<-suijinadatadfx[complete.cases(suijinadatadfx),]
        whichpro3<-grep(targettextx,rownames(suijinadatadfx2),perl = TRUE)
        #
        targetdfall<-NULL
        targetnanum1<-targetnanum2<-0
        if(length(whichpro1)>0){
          shuju1<-reshape2::melt(t(yuanshidf1[whichpro1,]))
          shuju1$Group<-"Original"
          whichpro1name<-rownames(yuanshidf1)[whichpro1]
          whichprox1df<-yuanshiimputlist[[selectmethod]]
          whichprox1df1<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
          shuju2<-reshape2::melt(t(whichprox1df1))
          shuju2$Group<-"Imputed"
          shuju3<-rbind(shuju1,shuju2)
          targetdfall<-rbind(targetdfall,shuju3)
          targetnanum1<-max(apply(yuanshidf1[whichpro1,],1,function(x){sum(is.na(x))}))
        }
        if(length(whichpro2)>0){
          whichpro1name<-rownames(suijinadatadfx1)[whichpro2]
          shuju1<-reshape2::melt(t(datanonaoutx[rownames(datanonaoutx)%in%whichpro1name,]))
          shuju1$Group<-"Original"
          whichprox1df<-suijidatajieguo[[selectmethod]]
          whichprox1df2<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
          shuju2<-reshape2::melt(t(whichprox1df2))
          shuju2$Group<-"Imputed"
          shuju3<-rbind(shuju1,shuju2)
          targetdfall<-rbind(targetdfall,shuju3)
          targetnanum2<-max(apply(suijinadatadfx1[whichpro2,],1,function(x){sum(is.na(x))}))
        }
        if(is.null(targetdfall)){
          if(length(whichpro3)>0){
            shuju1<-reshape2::melt(t(suijinadatadfx2[whichpro3,]))
            shuju1$Group<-"Original"
            whichpro1name<-rownames(suijinadatadfx2)[whichpro3]
            whichprox1df<-suijidatajieguo[[selectmethod]]
            whichprox1df2<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
            shuju2<-reshape2::melt(t(whichprox1df2))
            shuju2$Group<-"Imputed"
            shuju3<-rbind(shuju1,shuju2)
            targetdfall<-rbind(targetdfall,shuju3)
            targetdfall$Group<-factor(targetdfall$Group,levels = c("Original","Imputed"))
            ggplot(data=targetdfall, aes(x=Var1, y=value, fill=Group)) +
              geom_bar(stat="identity", color="black", position=position_dodge())+
              theme_bw()+
              facet_wrap(~Var2,ncol = 2)+
              theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.3),
                    plot.title = element_text(size=18),
                    plot.subtitle=element_text(size=15),
                    strip.text.x = element_text(size = 13))+
              labs(title="Target protein/peptide was not missed in any sample.",
                   subtitle=paste0("Imputation method: ",selectmethod),
                   x="Samples",y="Values")
          }else{
            plot(0,xlim=c(-1,1),ylim=c(-1,1), xaxt = "n", bty = "n", yaxt = "n", type = "n", xlab = "", ylab = "")
            text(0,0.5,labels = "Target protein/peptide not found. Please make sure the item is included in the input table!",cex=2)
          }
        }else{
          if(length(whichpro3)>0){
            shuju1<-reshape2::melt(t(suijinadatadfx2[whichpro3,]))
            shuju1$Group<-"Original"
            whichpro1name<-rownames(suijinadatadfx2)[whichpro3]
            whichprox1df<-suijidatajieguo[[selectmethod]]
            whichprox1df2<-whichprox1df[rownames(whichprox1df)%in%whichpro1name,]
            shuju2<-reshape2::melt(t(whichprox1df2))
            shuju2$Group<-"Imputed"
            shuju3<-rbind(shuju1,shuju2)
            targetdfall<-rbind(targetdfall,shuju3)
          }
          targetdfall$Group<-factor(targetdfall$Group,levels = c("Original","Imputed"))
          ggplot(data=targetdfall, aes(x=Var1, y=value, fill=Group)) +
            geom_bar(stat="identity", color="black", position=position_dodge())+
            theme_bw()+
            facet_wrap(~Var2,ncol = 2)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.3),
                  plot.title = element_text(size=16),
                  plot.subtitle=element_text(size=14),
                  strip.text.x = element_text(size = 13))+
            labs(title=paste0("Target protein/peptide was missed in N=",max(c(targetnanum1,targetnanum2)),
                              " samples among all N=",ncol(yuanshidf)," samples"),
                 subtitle=paste0("Imputation method: ",selectmethod),
                 x="Samples",y="Values")
        }
      })
      output$targettextplotdl<-downloadHandler(
        filename = function(){paste("TargetedCheck_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = pinjiafigheightx()/100+2,height = pinjiafigheightx()/100)
          print(targettextplotout())
          dev.off()
        }
      )
    }
  )

})

shinyApp(ui = ui, server = server)
