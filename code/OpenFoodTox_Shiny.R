# Title and definition ----------------------------------------------------

# Web version of the final function to extract data from OpenFoodTox.xlsx.
# It is created using Shiny

# Regarding upload to the web: 
#   https://www.shinyapps.io/
#   https://shiny.rstudio.com/tutorial/written-tutorial/lesson7/

# Out of Shiny: Libraries and data ----------------------------------------

# 1) Libraries

library(DT)        # To modify the style of tables: https://rstudio.github.io/DT/
library(readxl)    # Functions to load the xlsx files
library(openxlsx)  # To load the xlsx file from URL
library(shiny)     # To create Shiny App
library(shinyFiles)# To create new tables in Shiny
library(stringr)   # Functions to include in search both capital letters and not
library(plyr)      # For ldply in pubchem chunk. Its to did a "for" loop creating a data frame.
library(tidyverse) # Functions to extract the data
library(webchem)   # To include information from PubChem


# 2) Load data

# Option 1: From URL

#Index <- read.xlsx("https://zenodo.org/record/4740174/files/OpenFoodToxTx22761_2021.xlsx?download=1", sheet = "STUDY") # Load the datasheet
#Index = Index %>% select("SUB_COM_ID","OP_ID","TOX_ID","GENOTOX_ID","HAZARD_ID") # Select columns
#names(Index) = c("SUB_COM_ID","Output_Id","TOX_ID","GENOTOX_ID","HAZARD_ID") # Change of the name of columns to names more comprehensible

#Component <- read.xlsx("https://zenodo.org/record/4740174/files/OpenFoodToxTx22761_2021.xlsx?download=1", sheet = "COMPONENT")
#Component <- Component %>% select(SUB_COM_ID,SUB_NAME,SUBPARAMNAME,QUALIFIER,COM_NAME,SUB_CASNUMBER,SUB_ECSUBINVENTENTRYREF,MOLECULARFORMULA,SMILESNOTATION,INCHI,IUPACNAME)
#names(Component) = c("SUB_COM_ID","Substance","Description","has","Component","CAS_number", "EC_Ref_No", "Molecular_formula", "Smiles", "INCHI", "IUPAC_Name")

#Opinion <- read.xlsx("https://zenodo.org/record/4740174/files/OpenFoodToxTx22761_2021.xlsx?download=1", sheet = "OPINION")
#Opinion <- Opinion %>% select(AUTHOR,PUBLICATIONDATE,OP_ID,TITLE,DOCTYPE,REGULATION,URL)
#names(Opinion) = c("Author","Published","Output_Id","Title", "Output_Type", "Legal_Basis", "URL")

#Refer_points <- read.xlsx("https://zenodo.org/record/4740174/files/OpenFoodToxTx22761_2021.xlsx?download=1", sheet = "ENDPOINTSTUDY")
#Refer_points <- Refer_points %>% select(TOX_ID,STUDY_CATEGORY,TESTTYPE,SPECIES,ROUTE,EXP_DURATION_DAYS,ENDPOINT,QUALIFIER,VALUE,DOSEUNIT,BASIS,TOXICITY)
#names(Refer_points) = c("TOX_ID","Study","Test_Type", "Species", "Route", "Duration_(Days)", "Endpoint","Qualifier","Value_Reference_Point","Unit_Reference_Point","Effect","Toxicity")

#Refer_values <- read.xlsx("https://zenodo.org/record/4740174/files/OpenFoodToxTx22761_2021.xlsx?download=1", sheet = "CHEM_ASSESS")
#Refer_values <- Refer_values %>% select(HAZARD_ID,ASSESSMENTTYPE,RISKVALUE,RISKUNIT,POPULATIONTEXT,REMARKS)
#names(Refer_values) = c("HAZARD_ID","Assessment","Value_Reference_Value","Unit_Reference_Value","Population","Remarks")

#Genotox <- read.xlsx("https://zenodo.org/record/4740174/files/OpenFoodToxTx22761_2021.xlsx?download=1", sheet = "GENOTOX")
#Genotox <- Genotox %>% select(GENOTOX_ID,IS_GENOTOXIC)
#names(Genotox) = c("GENOTOX_ID","Genotoxicity")


# Option 2: From file

Index <- read.xlsx("OpenFoodToxTx22761_2021.xlsx", sheet = "STUDY") # Load the datasheet
Index = Index %>% select("SUB_COM_ID","OP_ID","TOX_ID","GENOTOX_ID","HAZARD_ID") # Select columns
names(Index) = c("SUB_COM_ID","Output_Id","TOX_ID","GENOTOX_ID","HAZARD_ID") # Change of the name of columns to names more comprehensible

Component <- read.xlsx("OpenFoodToxTx22761_2021.xlsx", sheet = "COMPONENT")
Component <- Component %>% select(SUB_COM_ID,SUB_NAME,SUBPARAMNAME,QUALIFIER,COM_NAME,SUB_CASNUMBER,SUB_ECSUBINVENTENTRYREF,MOLECULARFORMULA,SMILESNOTATION,INCHI,IUPACNAME)
names(Component) = c("SUB_COM_ID","Substance","Description","has","Component","CAS_number", "EC_Ref_No", "Molecular_formula", "Smiles", "INCHI", "IUPAC_Name")

Opinion <- read.xlsx("OpenFoodToxTx22761_2021.xlsx", sheet = "OPINION")
Opinion <- Opinion %>% select(AUTHOR,PUBLICATIONDATE,OP_ID,TITLE,DOCTYPE,REGULATION,URL)
names(Opinion) = c("Author","Published","Output_Id","Title", "Output_Type", "Legal_Basis", "URL")

Refer_points <- read.xlsx("OpenFoodToxTx22761_2021.xlsx", sheet = "ENDPOINTSTUDY")
Refer_points <- Refer_points %>% select(TOX_ID,STUDY_CATEGORY,TESTTYPE,SPECIES,ROUTE,EXP_DURATION_DAYS,ENDPOINT,QUALIFIER,VALUE,DOSEUNIT,BASIS,TOXICITY)
names(Refer_points) = c("TOX_ID","Study","Test_Type", "Species", "Route", "Duration_(Days)", "Endpoint","Qualifier","Value_Reference_Point","Unit_Reference_Point","Effect","Toxicity")

Refer_values <- read.xlsx("OpenFoodToxTx22761_2021.xlsx", sheet = "CHEM_ASSESS")
Refer_values <- Refer_values %>% select(HAZARD_ID,ASSESSMENTTYPE,RISKVALUE,RISKUNIT,POPULATIONTEXT,REMARKS)
names(Refer_values) = c("HAZARD_ID","Assessment","Value_Reference_Value","Unit_Reference_Value","Population","Remarks")

Genotox <- read.xlsx("OpenFoodToxTx22761_2021.xlsx", sheet = "GENOTOX")
Genotox <- Genotox %>% select(GENOTOX_ID,IS_GENOTOXIC)
names(Genotox) = c("GENOTOX_ID","Genotoxicity")

# 3) To select columns -----------------------------------------------------------------

list_of_columns = c("Substance","Description","Component","CAS_number","EC_Ref_No","Molecular_formula","Smiles","INCHI","IUPAC_Name","has","Output_Id","Author","Published","Title","Output_Type","Legal_Basis","URL","Study","Test_Type","Species","Route","Duration_(Days)","Endpoint","Qualifier","Value_Reference_Point","Unit_Reference_Point","Effect","Toxicity","Assessment","Value_Reference_Value","Unit_Reference_Value","Population","Remarks","Genotoxicity","PubChem_CID","PubChem_Link")        
# The names of the different columns to select.


# 4) Shiny -----------------------------------------------------------------

css <- ".nowrap {white-space: nowrap;}" # Format-style to make that text in table in the screens be in only one line.


# 3.1) UI: User interface, what you see in the screen ------------------------



ui <- fluidPage(
  
  tags$head(tags$style(HTML(css))), # To use the format css
  
  h1(id="Title", "Web function to extract data from OpenFoodTox in txt format"), # Title
  tags$style(HTML("#Title{color: CornflowerBlue;font-size: 40px;font-weight: bold}")), # Characteristics of title
  
  sidebarLayout( # Forta of the web, with inputs in the left side, and the results in the rest.
    sidebarPanel(
      actionButton("go", "Go!", icon("dungeon"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), # Button to start the search
      downloadLink('downloadData', 'Download'), # Button to download the result
      tags$p(), # To introduce a white row (aesthetic)
      selectInput("select", "Select columns to display", list_of_columns, multiple = TRUE), # To select the columns
      hr(style = "border-top: 1px solid #000000;"), # Line to divide
      textInput(inputId = "a",label = "Write the name of the compound (1)", value = ""), # Value indicates what appears in the box (the default value)
      textInput(inputId = "b",label = "Write the name of the compound (2)", value = ""),
      textInput(inputId = "c",label = "Write the name of the compound (3)", value = ""),
      textInput(inputId = "d",label = "Write the name of the compound (4)", value = ""),
      textInput(inputId = "e",label = "Write the name of the compound (5)", value = ""),
      textInput(inputId = "f",label = "Write the name of the compound (6)", value = ""),
      textInput(inputId = "g",label = "Write the name of the compound (7)", value = ""),
      textInput(inputId = "h",label = "Write the name of the compound (8)", value = ""),
      textInput(inputId = "i",label = "Write the name of the compound (9)", value = ""),
      textInput(inputId = "j",label = "Write the name of the compound (10)", value = ""),
      textInput(inputId = "k",label = "Write the name of the compound (11)", value = ""),
      textInput(inputId = "l",label = "Write the name of the compound (12)", value = ""),
      textInput(inputId = "m",label = "Write the name of the compound (13)", value = ""),
      textInput(inputId = "n",label = "Write the name of the compound (14)", value = ""),
      textInput(inputId = "o",label = "Write the name of the compound (15)", value = ""), width = 2
    ),
    
    mainPanel(
      verbatimTextOutput("version"), # Indicates the version of the database
      verbatimTextOutput("repeated"), # Indicate if some of the inputs are repeated
      hr(style = "border-top: 1px solid #000000;"), # Line to divide
      tableOutput("summary"), # # To indicate the different names of compounds found
      hr(style = "border-top: 1px solid #000000;"), # Line to divide
      dataTableOutput("complete") # The interactive table
    )
  )
)


server <- function(input, output, session) {
  
  # options(shiny.maxRequestSize=30*1024^2) 
  # This part is needed only if we indicate that the user have to upload the file, to make possible to upload such a big file.
  
  version = eventReactive(input$go,{cat("Version 4 of OpenFoodTox, 19/05/2021, file: OpenFoodToxTx22761_2021.xlsx")}) # This is just to show this text about the version only when the function runs.
  output$version = renderPrint(version())
  
  # Chunk 1: To delay the process until the push of the button.
  # EventReactive is to make that is necessary to push the button created above (actionButton "go") to start.
  
  new_input_a <- reactive({if(input$a == ""){"Empty"}else{input$a}}) # To make that the input file don´t shoy text, the dafault value is nothing, but if you search for it,
  new_input_b <- reactive({if(input$b == ""){"Empty"}else{input$b}}) # the function returns the entire table. Here we substitute the input value by "Empty", or the value
  new_input_c <- reactive({if(input$c == ""){"Empty"}else{input$c}}) # if something is written. 
  new_input_d <- reactive({if(input$d == ""){"Empty"}else{input$d}})
  new_input_e <- reactive({if(input$e == ""){"Empty"}else{input$e}})
  new_input_f <- reactive({if(input$f == ""){"Empty"}else{input$f}})
  new_input_g <- reactive({if(input$g == ""){"Empty"}else{input$g}})
  new_input_h <- reactive({if(input$h == ""){"Empty"}else{input$h}})
  new_input_i <- reactive({if(input$i == ""){"Empty"}else{input$i}})
  new_input_j <- reactive({if(input$j == ""){"Empty"}else{input$j}})
  new_input_k <- reactive({if(input$k == ""){"Empty"}else{input$k}})
  new_input_l <- reactive({if(input$l == ""){"Empty"}else{input$l}})
  new_input_m <- reactive({if(input$m == ""){"Empty"}else{input$m}})
  new_input_n <- reactive({if(input$n == ""){"Empty"}else{input$n}})
  new_input_o <- reactive({if(input$o == ""){"Empty"}else{input$o}})
  
  input_value_a <- eventReactive(input$go, {new_input_a()}) # With this, we can delay the search until press "Go"
  input_value_b <- eventReactive(input$go, {new_input_b()})
  input_value_c <- eventReactive(input$go, {new_input_c()})
  input_value_d <- eventReactive(input$go, {new_input_d()})
  input_value_e <- eventReactive(input$go, {new_input_e()})
  input_value_f <- eventReactive(input$go, {new_input_f()})
  input_value_g <- eventReactive(input$go, {new_input_g()})
  input_value_h <- eventReactive(input$go, {new_input_h()})
  input_value_i <- eventReactive(input$go, {new_input_i()})
  input_value_j <- eventReactive(input$go, {new_input_j()})
  input_value_k <- eventReactive(input$go, {new_input_k()})
  input_value_l <- eventReactive(input$go, {new_input_l()})
  input_value_m <- eventReactive(input$go, {new_input_m()})
  input_value_n <- eventReactive(input$go, {new_input_n()})
  input_value_o <- eventReactive(input$go, {new_input_o()})
  
  
  # Chunk 3: Checking repeated values in input
  
  selection <- eventReactive(input$go, {
    c(new_input_a(),new_input_b(),new_input_c(),new_input_d(),new_input_e(),new_input_f(),new_input_g(),new_input_h(),new_input_i(),new_input_j(),new_input_k(),new_input_l(),new_input_m(),new_input_n(),new_input_o()) 
  }) # Chain with all the inputs
  output$repeated <- renderPrint({ # renderPrint prints text in the web.
    selection2 <- as.data.frame(selection()) # To transfor the selection in a dataframe to further steps.
    selection3 <- selection2[!(selection2=="Empty"),] %>% as.data.frame() # Eliminate "Empty" values
    duplicated = selection3[duplicated(selection3),] # To writhe which values are repeated.
    
    if(nrow(selection3)==nrow(distinct(selection3))){ # In the dataframe without Empty, check if the number of rows are the same if duplicated values are eliminated.
      cat("None of the compounds names are repeated.")
    } else {
      cat("The value(s) ", duplicated, " is(are) repeated. Repeated terms are not included in the search.")
    }
    
  })
  
  # Chunk 4: Filtering data
  
  # Reactive is needed when the output is going to be used as input in other parts.
  
  Selection_a = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_a())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_a()))}) # Filtering sheet "Compound" by two columns.
  
  Selection_b = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_b())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_b()))})
  
  Selection_c = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_c())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_c()))})
  
  Selection_d = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_d())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_d()))})
  
  Selection_e = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_e())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_e()))})
  
  Selection_f = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_f())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_f()))})
  
  Selection_g = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_g())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_g()))})
  
  Selection_h = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_h())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_h()))})
  
  Selection_i = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_i())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_i()))})
  
  Selection_j = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_j())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_j()))})
  
  Selection_k = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_k())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_k()))})
  
  Selection_l = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_l())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_l()))})
  
  Selection_m = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_m())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_m()))})
  
  Selection_n = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_n())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_n()))})
  
  Selection_o = reactive({stringr::str_detect(str_to_lower(Component$Substance),str_to_lower(input_value_o())) | stringr::str_detect(str_to_lower(Component$Description),str_to_lower(input_value_o()))})
  
  
  filter_abc = reactive({rbind(Component[Selection_a(),], Component[Selection_b(),], Component[Selection_c(),],Component[Selection_d(),],Component[Selection_e(),],Component[Selection_f(),],Component[Selection_g(),],Component[Selection_h(),],Component[Selection_i(),],Component[Selection_j(),],Component[Selection_k(),],Component[Selection_l(),],Component[Selection_m(),],Component[Selection_n(),],Component[Selection_o(),])}) # This is to create the table which joins Substance sheet filtered by all compounds.
  
  summary_results <- reactive({
    summary_results = distinct(as.data.frame((filter_abc()$Substance)))
    colnames(summary_results) = "Compounds found"
    summary_results
  }) # To create a dataframe with a name of columns previously selected.
  
  output$summary <- renderTable({
    summary_results()
  }) # To print the previous dataframe.
  
  pubchem <- reactive({
    summary_results = distinct(as.data.frame((filter_abc()$Component)))
    colnames(summary_results) = "Component"
    CID_values = ldply(summary_results$Component,get_cid)
    CID_values = CID_values %>% filter(cid != "NA") # To avoid have a link to "Sodium" if you don´t find the CID (because the result is "NA")
    CID_values$URL = paste("https://pubchem.ncbi.nlm.nih.gov/compound/",CID_values$cid)
    CID_values$URL = gsub(" ","",CID_values$URL)
    CID_values$URL = paste0("<a href='",CID_values$URL,"' target='_blank'>",CID_values$URL,"</a>")
    colnames(CID_values) = c("Component","PubChem_CID","PubChem_Link")
    CID_values
  }) # To create a dataframe with the CID and the URL of PubChem, based on the value "Compound". The problem is when the value is not found in the OpenFoodTox, it appears as "NA", and the link goes to "Sodium".
  
  
  final_table <- reactive({
    Complete_1 <- left_join(data.frame(filter_abc()), Index, by = "SUB_COM_ID")
    Complete_2 <- left_join(Complete_1, Opinion, by = "Output_Id")
    Complete_3 <- left_join(Complete_2, Refer_points, by = "TOX_ID")
    Complete_4 <- left_join(Complete_3, Refer_values, by = "HAZARD_ID")
    Complete_5 <- left_join(Complete_4, Genotox, by = "GENOTOX_ID")
    Complete_6 <- left_join(Complete_5, pubchem(), by = "Component")
    Complete_7 <- select(Complete_6, -c(SUB_COM_ID,TOX_ID,GENOTOX_ID,HAZARD_ID)) # Here we eliminate internal identificators, which in addition allows to eliminate some rows.
    Complete_8 <- distinct(Complete_7) # To eliminate duplicate rows.
    Complete_9 <- Complete_8[,c(1,2,4,5,6,7,8,9,10,3,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)] # To relocate the columns to start with the names and identificator of the compounds.
  })
  
  output$complete <- reactive({
    datatable(final_table(), options = list(columnDefs = list(list(className = "nowrap", targets = "_all"))))}) # The final table in the screen. The options are to make that the text be in only one line.
  
  mytable = reactive({
    columns = names(final_table())
    if (!is.null(input$select)) {
      columns = input$select
    }
    final_table()[,columns,drop=FALSE]
  }) # This creates a table based on selected columns
  
  output$complete <- renderDataTable({
    datatable(mytable(), options = list(columnDefs = list(list(className = "nowrap", targets = "_all"))),escape = FALSE)}) # The final table in the screen. The options are to make that the text be in only one line.
  
  
  name_file = paste0(Sys.time(),"_file_txt") # This three lines are to save the file with the date and time.
  name_file = gsub(":","-",name_file)
  name_file = gsub(" ","_",name_file)
  
  output$downloadData <- downloadHandler( # Which happen when you push the "dowload" button. 
    filename = function() {
      name_file = paste0(Sys.time(),"_file_txt")
      name_file = gsub(":","-",name_file)
      name_file = gsub(" ","_",name_file)
    },
    content = function(file) {
      write.table(final_table(), file, sep = "\t",row.names = FALSE)
    } # To save the function.
    
  )
  
  
  
}

shinyApp(ui = ui, server = server)