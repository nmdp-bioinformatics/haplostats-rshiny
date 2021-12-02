shinyServer(function(input, output, session) {
  
  # Logged In User Section --------------------------------------------------
  username <- reactive({
    session$user
  })
  
  output$username <- renderUI({
    username <- username() 
    if (is.null(username)) return("Not Logged In")
    span("Logged in as", strong(username))
  })
  
  # Instructions Section --------------------------------------------------
  
  
  resetPageFull <- eventReactive(input$resetPageFull,{
    isolate({
      updateTextInput(session, "A1",label='',value = '')
      updateTextInput(session, "A2",label='',value = '') 
      updateTextInput(session, "B1",label='',value = '') 
      updateTextInput(session, "B2",label='',value = '') 
      updateTextInput(session, "C1",label='',value = '') 
      updateTextInput(session, "C2",label='',value = '') 
      updateTextInput(session, "DRB1_1",label='',value = '') 
      updateTextInput(session, "DRB1_2",label='',value = '') 
      updateTextInput(session, "DQB1_1",label='',value = '') 
      updateTextInput(session, "DQB1_2",label='',value = '') 
      updateTextInput(session, "DRB3_1",label='',value = '') 
      updateTextInput(session, "DRB3_2",label='',value = '')
      updateTextInput(session, "DRB4_1",label='',value = '') 
      updateTextInput(session, "DRB4_2",label='',value = '')
      updateTextInput(session, "DRB5_1",label='',value = '') 
      updateTextInput(session, "DRB5_2",label='',value = '')
      updateCheckboxGroupInput(session,'population_group',selected = '')
      updateCheckboxGroupInput(session,'AFApopulation_group',selected = '')
      updateCheckboxGroupInput(session,'HISpopulation_group',selected = '')
      updateCheckboxGroupInput(session,'APIpopulation_group',selected = '')
      updateCheckboxGroupInput(session,'NAMpopulation_group',selected = '')
      updateCheckboxGroupInput(session,'CAUpopulation_group',selected = '')
      updateTextInput(session, "ID",value = '')
      
    })
  })
  observe({
    resetPageFull()
  })

  # Data Selection Section --------------------------------------------------
  
  moveToResults <- eventReactive(input$submit_query, {
    isolate({
      mug <- validateMUG()
      
      if (!is.null(mug)) {
          if(length(mug) > 0) {
              updateTabsetPanel(session, "mainMenu", selected = "resultstab")
          }
      }
    })
  })
  observe({
    moveToResults()
  })
  
  
  ## data set
  getDataset<- eventReactive(input$submit_query, {
    isolate({
      return (input$dataset)
    })
    
  })
  
  ## loci
  getLoci<- eventReactive(input$submit_query, {
    isolate({
      return (input$loci)
    })
    
  })
  
  ## id
  
  runID <- reactive({
    input$ID_lookup   
  })
  
  getID <- eventReactive(input$ID_lookup, {
    isolate({
      return (input$ID)
    })
  })
  
  validateID <- reactive({
    
    if (runID() == 0) return(NULL)    
    
    # Don't get the ID until the button is clicked.
    isolate({
      id <- gsub('-','',getID())
      id <- gsub(' ','',id)
    })
    
    if (nchar(id)==7 & input$id_type=='patient'){
      return(as.numeric(id))
    } else if (nchar(id)==9 & input$id_type=='donor'){
      return(as.numeric(id))
    } else if (nchar(id)==9 & input$id_type=='cord'){
      return(as.numeric(id))
    } else if (nchar(id)==32 & input$id_type=='guid'){
      return(as.character(id))
    } else {
      return("invalid")
    }
    
    
  })
  
  output$id_invalid <- renderUI({
    
    id <- validateID()
    
    if (id == "invalid") {
      p("ID is invalid", style = "color:red")
    } else {
      ""
    }    
  })
  
  lookupID <- reactive({
    if (runID() == 0) return(NULL)    
    
    id <- validateID() 
    
    if (!is.null(id) && id != "invalid") {
      
      mug <- mugForID()
      
      if (!is.null(mug)) {
        id
      } else {
        "notfound"    
      }     
    }
    
  })
  
  mugForID <- reactive({
    
    if (runID() == 0) return(NULL)    
    id <- validateID() 
    
    if (id!='invalid'){
      if (input$id_type=='patient') {
        return(getMugForRID(id,HAPLOTYPE_HOST_ADDRESS)[[1]])
      } else if (input$id_type=='donor') {
        return(getMugForDID(id,HAPLOTYPE_HOST_ADDRESS)[[1]])
      } else if (input$id_type=='cord') {
        return(getMugForCID(id,HAPLOTYPE_HOST_ADDRESS)[[1]])
      } else if (input$id_type=='guid'){
        return(getMugForGuid(id,HAPLOTYPE_HOST_ADDRESS)[[1]])
      } else {
        return(empty_mug)
      }
    }
    print('Invalid ID')
    
    NULL
  })
  
  populationForID <- reactive({
    
    if (runID() == 0) return(NULL)    
    id <- validateID() 
    
    if (id!='invalid'){
      if (input$id_type=='patient') {
        return(getDetailedForRID(id,HAPLOTYPE_HOST_ADDRESS)[3])
      } else if (input$id_type=='donor') {
        return(getDetailedForDID(id,HAPLOTYPE_HOST_ADDRESS)[3])
      } else if (input$id_type=='cord') {
        return(getDetailedForCID(id,HAPLOTYPE_HOST_ADDRESS)[3])
      } else if (input$id_type=='guid'){
        return(getDetailedForGuid(id,HAPLOTYPE_HOST_ADDRESS)[3])
      } else {
        return('')
      }
    }
    print('Invalid ID')
    
    NULL
  })
  
  observe({
    mug <- mugForID()
    pop <- populationForID()
    tablemug <- formatListToTable(mug)
    
    updateTextInput(session, "A1",label='',value = tablemug[1,2])
    updateTextInput(session, "A2",label='',value = tablemug[1,3]) 
    updateTextInput(session, "B1",label='',value = tablemug[2,2]) 
    updateTextInput(session, "B2",label='',value = tablemug[2,3]) 
    updateTextInput(session, "C1",label='',value = tablemug[3,2]) 
    updateTextInput(session, "C2",label='',value = tablemug[3,3]) 
    updateTextInput(session, "DRB1_1",label='',value = tablemug[4,2]) 
    updateTextInput(session, "DRB1_2",label='',value = tablemug[4,3]) 
    updateTextInput(session, "DQB1_1",label='',value = tablemug[5,2]) 
    updateTextInput(session, "DQB1_2",label='',value = tablemug[5,3]) 
    updateTextInput(session, "DRB3_1",label='',value = tablemug[6,2]) 
    updateTextInput(session, "DRB3_2",label='',value = tablemug[6,3])
    updateTextInput(session, "DRB4_1",label='',value = tablemug[7,2]) 
    updateTextInput(session, "DRB4_2",label='',value = tablemug[7,3])
    updateTextInput(session, "DRB5_1",label='',value = tablemug[8,2]) 
    updateTextInput(session, "DRB5_2",label='',value = tablemug[8,3])
    
    if(!is.null(pop)){
      if (pop %in% BROAD){
        updateCheckboxGroupInput(session,'population_group',selected = pop)
      }
      if (pop %in% AFA){
        updateCheckboxGroupInput(session,'AFApopulation_group',selected = pop)
      }
      if (pop %in% HIS){
        updateCheckboxGroupInput(session,'HISpopulation_group',selected = pop)
      }
      if (pop %in% API){
        updateCheckboxGroupInput(session,'APIpopulation_group',selected = pop)
      }
      if (pop %in% NAM){
        updateCheckboxGroupInput(session,'NAMpopulation_group',selected = pop)
      }
      if (pop %in% CAU){
        updateCheckboxGroupInput(session,'CAUpopulation_group',selected = pop)
      }
    }
    
  })
  
  output$error_box <- renderUI ({
    
    id <- lookupID()
    id2 <- validateID()
    if (!is.null(id) && id == "notfound") {
      return(div(strong("Sorry, ID was not found."),br(),"If you have the typings, try entering them manually", style = "color:red"))
    }
    
    if (!is.null(id2) && id2 == "invalid") {
      return(div("ID is invalid", style = "color:red"))
    }
    
    ""
    
  })
  
  
  
  ## mug
  validateMUG <- reactive({
    
    mug <- extractMUG()
    loci <- getLoci()
    if(length(mug) > 0) {
      if (loci!='C~B'){
      
          lociList <- sapply(mug, FUN=function(c) c$locus)
          if (all(c("A","B") %in% lociList)){      
            return(mug)    
          }
      } else {
        lociList <- sapply(mug, FUN=function(c) c$locus)
        if (all(c("C","B") %in% lociList)){      
          return(mug)    
        }
        
      }
    }
    
    list()    
  })
  
  extractMUG <- eventReactive(input$submit_query, {
    
    isolate({
      HLA<-list()
      ind<-1
      
      if(input$A1!=''){  
        
        if (isSerology(input$A1)){
          type1 = input$A1
        } else {
          type1 = toupper(input$A1)
        }
        if (isSerology(input$A2)){
          type2 = input$A2
        } else {
          type2 = toupper(input$A2)
        }
        
        updateTextInput(session, "A1",label='',value = type1)
        updateTextInput(session, "A2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'A', type1 = type1, type2 = type2)
        ind<-ind+1
      } 
      
      if(input$B1!=''){  
        
        if (isSerology(input$B1)){
          type1 = input$B1
        } else {
          type1 = toupper(input$B1)
        }
        if (isSerology(input$B2)){
          type2 = input$B2
        } else {
          type2 = toupper(input$B2)
        }
        
        updateTextInput(session, "B1",label='',value = type1)
        updateTextInput(session, "B2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'B', type1 = type1, type2 = type2)
        ind<-ind+1
      } 
      
      if(input$C1!=''){  
        if (isSerology(input$C1)){
          type1 = input$C1
        } else {
          type1 = toupper(input$C1)
        }
        if (isSerology(input$C2)){
          type2 = input$C2
        } else {
          type2 = toupper(input$C2)
        }
        
        updateTextInput(session, "C1",label='',value = type1)
        updateTextInput(session, "C2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'C', type1 = type1, type2 = type2)
        ind<-ind+1
      } 
      
      if(input$DRB1_1!=''){  
        if (isSerology(input$DRB1_1)){
          type1 = input$DRB1_1
        } else {
          type1 = toupper(input$DRB1_1)
        }
        if (isSerology(input$DRB1_2)){
          type2 = input$DRB1_2
        } else {
          type2 = toupper(input$DRB1_2)
        }
        
        updateTextInput(session, "DRB1_1",label='',value = type1)
        updateTextInput(session, "DRB1_2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'DRB1', type1 = type1, type2 = type2)
        ind<-ind+1
      } 
      
      if(input$DQB1_1!=''){  
        if (isSerology(input$DQB1_1)){
          type1 = input$DQB1_1
        } else {
          type1 = toupper(input$DQB1_1)
        }
        if (isSerology(input$DQB1_2)){
          type2 = input$DQB1_2
        } else {
          type2 = toupper(input$DQB1_2)
        }
        
        updateTextInput(session, "DQB1_1",label='',value = type1)
        updateTextInput(session, "DQB1_2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'DQB1', type1 = type1, type2 = type2)
        ind<-ind+1
      } 
      if(input$DRB3_1!=''){  
        if (isSerology(input$DRB3_1)){
          type1 = input$DRB3_1
        } else {
          type1 = toupper(input$DRB3_1)
        }
        if (isSerology(input$DRB3_2)){
          type2 = input$DRB3_2
        } else {
          type2 = toupper(input$DRB3_2)
        }
        
        updateTextInput(session, "DRB3_1",label='',value = type1)
        updateTextInput(session, "DRB3_2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'DRB3', type1 = type1, type2 = type2)
        ind<-ind+1
      }
      if(input$DRB4_1!=''){  
        if (isSerology(input$DRB4_1)){
          type1 = input$DRB4_1
        } else {
          type1 = toupper(input$DRB4_1)
        }
        if (isSerology(input$DRB4_2)){
          type2 = input$DRB4_2
        } else {
          type2 = toupper(input$DRB4_2)
        }
        
        updateTextInput(session, "DRB4_1",label='',value = type1)
        updateTextInput(session, "DRB4_2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'DRB4', type1 = type1, type2 = type2)
        ind<-ind+1
      }
      if(input$DRB5_1!=''){  
        if (isSerology(input$DRB5_1)){
          type1 = input$DRB5_1
        } else {
          type1 = toupper(input$DRB5_1)
        }
        if (isSerology(input$DRB5_2)){
          type2 = input$DRB5_2
        } else {
          type2 = toupper(input$DRB5_2)
        }
        
        updateTextInput(session, "DRB5_1",label='',value = type1)
        updateTextInput(session, "DRB5_2",label='',value = type2)
        
        HLA[[ind]] <- list(locus = 'DRB5', type1 = type1, type2 = type2)
        ind<-ind+1
      }
      
      return(HLA)
    })
    
  })
  
  output$mug_invalid <- renderUI({
    
    mug<-NULL
    
    mug <- validateMUG()
    
    if (is.null(mug)) {
      return(NULL)
    }
    
    # imputationError <- imputePairForMug(sire, mug,HAPLOTYPE_HOST_ADDRESS)$errorStatus 
    
    error_message <- ""
    error_message2 <- ""
    warning_message1 <- ""
    warning_message2 <- ""
    
    continue_message <- ""
    
    if(length(mug) == 0) {
      error_message <- "Error: HLA Typing must include A and B. <br>" 
    }
    
#     if(imputationError){
#       error_message2 <- "Error: Imputation failed. Please check your entered typing. Allele(s) or allele code(s) may not exist. You may need to convert allele codes to alleles.  <br>"
#     }
#     
#     if(!is.null(mug) && length(mug) > 0 && length(mug) < 5) {
#       warning_message1 <-  "Warning: Missing loci are being imputed, results may be less reliable.  <br>"
#     }
#     
#     if(!is.null(mug) && length(mug) > 0 && !isHighResolution(mug)) {
#       warning_message2 <- "Warning: Some loci are low resolution and are being imputed, results may be less reliable.  <br>"
#     }
    
    if(error_message=="" & error_message2==""){
      continue_message <-p("Typings are valid, please choose the next tab on the left.", style = "color:green")
    }
    
    
    
    div(p(HTML(paste(error_message,error_message2)), style = "color:red"),
        p(HTML(paste(warning_message1,warning_message2)), style = "color:orange"),
        continue_message)
    
  })
  
  example <- eventReactive(input$fillExample,{
    isolate({
      mugTable<-display_mug
      updateTextInput(session, "A1",label='',value = mugTable[1,2])
      updateTextInput(session, "A2",label='',value = mugTable[1,3]) 
      updateTextInput(session, "B1",label='',value = mugTable[2,2]) 
      updateTextInput(session, "B2",label='',value = mugTable[2,3]) 
      updateTextInput(session, "C1",label='',value = mugTable[3,2]) 
      updateTextInput(session, "C2",label='',value = mugTable[3,3]) 
      updateTextInput(session, "DRB1_1",label='',value = mugTable[4,2]) 
      updateTextInput(session, "DRB1_2",label='',value = mugTable[4,3]) 
      updateTextInput(session, "DQB1_1",label='',value = mugTable[5,2]) 
      updateTextInput(session, "DQB1_2",label='',value = mugTable[5,3]) 
      updateTextInput(session, "DRB3_1",label='',value = mugTable[6,2]) 
      updateTextInput(session, "DRB3_2",label='',value = mugTable[6,3])
      updateTextInput(session, "DRB4_1",label='',value = mugTable[7,2]) 
      updateTextInput(session, "DRB4_2",label='',value = mugTable[7,3])
      updateTextInput(session, "DRB5_1",label='',value = mugTable[8,2]) 
      updateTextInput(session, "DRB5_2",label='',value = mugTable[8,3])
    })
  })
  observe({
    example()
  })
  
  resetTyping <- eventReactive(input$resetTyping,{
    isolate({
      updateTextInput(session, "A1",label='',value = '')
      updateTextInput(session, "A2",label='',value = '') 
      updateTextInput(session, "B1",label='',value = '') 
      updateTextInput(session, "B2",label='',value = '') 
      updateTextInput(session, "C1",label='',value = '') 
      updateTextInput(session, "C2",label='',value = '') 
      updateTextInput(session, "DRB1_1",label='',value = '') 
      updateTextInput(session, "DRB1_2",label='',value = '') 
      updateTextInput(session, "DQB1_1",label='',value = '') 
      updateTextInput(session, "DQB1_2",label='',value = '') 
      updateTextInput(session, "DRB3_1",label='',value = '') 
      updateTextInput(session, "DRB3_2",label='',value = '')
      updateTextInput(session, "DRB4_1",label='',value = '') 
      updateTextInput(session, "DRB4_2",label='',value = '')
      updateTextInput(session, "DRB5_1",label='',value = '') 
      updateTextInput(session, "DRB5_2",label='',value = '')
    })
  })
  observe({
    resetTyping()
  })
  
  output$table_mug <- renderUI({
    
    mugTable<-empty_mug
    
    
    tags$table(
      tags$tr(
        tags$td(class = "mug", p('HLA-A', class = "locus-name")),
        tags$td(class = "mug", textInput('A1', label = '', value = mugTable[1,2])),
        tags$td(class = "mug", textInput('A2', label = '', value = mugTable[1,3]))
      ),
      tags$tr(
        tags$td(class = "mug", p('HLA-B', class = "locus-name")),
        tags$td(class = "mug", textInput('B1', label = '', value = mugTable[2,2])),
        tags$td(class = "mug", textInput('B2', label = '', value = mugTable[2,3]))
      ),
      tags$tr(
        tags$td(class = "mug", p('HLA-C', class = "locus-name")),
        tags$td(class = "mug", textInput('C1', label = '', value = mugTable[3,2])),
        tags$td(class = "mug", textInput('C2', label = '', value = mugTable[3,3]))
      ),
      tags$tr(
        tags$td(class = "mug", p('HLA-DRB1', class = "locus-name")),
        tags$td(class = "mug", textInput('DRB1_1', label = '', value = mugTable[4,2])),
        tags$td(class = "mug", textInput('DRB1_2', label = '', value = mugTable[4,3]))
      ),
      tags$tr(
        tags$td(class = "mug", p('HLA-DQB1', class = "locus-name")),
        tags$td(class = "mug", textInput('DQB1_1', label = '', value = mugTable[5,2])),
        tags$td(class = "mug", textInput('DQB1_2', label = '', value = mugTable[5,3]))
      ),
      tags$tr(
        tags$td(class = "mug", p('HLA-DRB3', class = "locus-name")),
        tags$td(class = "mug", textInput('DRB3_1', label = '', value = mugTable[6,2])),
        tags$td(class = "mug", textInput('DRB3_2', label = '', value = mugTable[6,3]))
      ),
      tags$tr(
        tags$td(class = "mug", p('HLA-DRB4', class = "locus-name")),
        tags$td(class = "mug", textInput('DRB4_1', label = '', value = mugTable[7,2])),
        tags$td(class = "mug", textInput('DRB4_2', label = '', value = mugTable[7,3]))
      ),
      tags$tr(
        tags$td(class = "mug", p('HLA-DRB5', class = "locus-name")),
        tags$td(class = "mug", textInput('DRB5_1', label = '', value = mugTable[8,2])),
        tags$td(class = "mug", textInput('DRB5_2', label = '', value = mugTable[8,3]))
      )
    )
    
    
  })
  
  ## populations
  
  getSIRE <- reactive ({
    broadsire <- input$population_group
    detailed_afa  <- input$AFApopulation_group
    detailed_api  <- input$APIpopulation_group
    detailed_cau  <- input$CAUpopulation_group
    detailed_his  <- input$HISpopulation_group
    detailed_nam  <- input$NAMpopulation_group
    
    return(c(broadsire,detailed_afa,detailed_api,detailed_cau,detailed_his,detailed_nam))
  })
  
  resetPop <- eventReactive(input$resetPop,{
    isolate({
      updateCheckboxGroupInput(session,'population_group',selected = '')
      updateCheckboxGroupInput(session,'AFApopulation_group',selected = '')
      updateCheckboxGroupInput(session,'HISpopulation_group',selected = '')
      updateCheckboxGroupInput(session,'APIpopulation_group',selected = '')
      updateCheckboxGroupInput(session,'NAMpopulation_group',selected = '')
      updateCheckboxGroupInput(session,'CAUpopulation_group',selected = '')
    })
  })
  observe({
    resetPop()
  })
  
  selectAllPop <- eventReactive(input$selectAllPop,{
    isolate({
      updateCheckboxGroupInput(session,'population_group',selected = BROAD)
      updateCheckboxGroupInput(session,'AFApopulation_group',selected = AFA)
      updateCheckboxGroupInput(session,'HISpopulation_group',selected = HIS)
      updateCheckboxGroupInput(session,'APIpopulation_group',selected = API)
      updateCheckboxGroupInput(session,'NAMpopulation_group',selected = NAM)
      updateCheckboxGroupInput(session,'CAUpopulation_group',selected = CAU)
    })
  })
  observe({
    selectAllPop()
  })
  
  getMugForPop <- reactive({
    
    HLA<-list()
    ind<-1
    
    if(input$A1!=''){  
      
      type1 = toupper(input$A1)
      type2 = toupper(input$A2)
      
      updateTextInput(session, "A1",label='',value = type1)
      updateTextInput(session, "A2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'A', type1 = type1, type2 = type2)
      ind<-ind+1
    } 
    
    if(input$B1!=''){  
      
      type1 = toupper(input$B1)
      type2 = toupper(input$B2)
      
      updateTextInput(session, "B1",label='',value = type1)
      updateTextInput(session, "B2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'B', type1 = type1, type2 = type2)
      ind<-ind+1
    } 
    
    if(input$C1!=''){  
      type1 = toupper(input$C1)
      type2 = toupper(input$C2)
      
      updateTextInput(session, "C1",label='',value = type1)
      updateTextInput(session, "C2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'C', type1 = type1, type2 = type2)
      ind<-ind+1
    } 
    
    if(input$DRB1_1!=''){  
      type1 = toupper(input$DRB1_1)
      type2 = toupper(input$DRB1_2)
      
      updateTextInput(session, "DRB1_1",label='',value = type1)
      updateTextInput(session, "DRB1_2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'DRB1', type1 = type1, type2 = type2)
      ind<-ind+1
    } 
    
    if(input$DQB1_1!=''){  
      type1 = toupper(input$DQB1_1)
      type2 = toupper(input$DQB1_2)
      
      updateTextInput(session, "DQB1_1",label='',value = type1)
      updateTextInput(session, "DQB1_2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'DQB1', type1 = type1, type2 = type2)
      ind<-ind+1
    } 
    if(input$DRB3_1!=''){  
      type1 = toupper(input$DRB3_1)
      type2 = toupper(input$DRB3_2)
      
      updateTextInput(session, "DRB3_1",label='',value = type1)
      updateTextInput(session, "DRB3_2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'DRB3', type1 = type1, type2 = type2)
      ind<-ind+1
    }
    if(input$DRB4_1!=''){  
      type1 = toupper(input$DRB4_1)
      type2 = toupper(input$DRB4_2)
      
      updateTextInput(session, "DRB4_1",label='',value = type1)
      updateTextInput(session, "DRB4_2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'DRB4', type1 = type1, type2 = type2)
      ind<-ind+1
    }
    if(input$DRB5_1!=''){  
      type1 = toupper(input$DRB5_1)
      type2 = toupper(input$DRB5_2)
      
      updateTextInput(session, "DRB5_1",label='',value = type1)
      updateTextInput(session, "DRB5_2",label='',value = type2)
      
      HLA[[ind]] <- list(locus = 'DRB5', type1 = type1, type2 = type2)
      ind<-ind+1
    }
    
    return(HLA)
    
  })
  
  identifyPop <- eventReactive(input$identifyPop,{
    
    mug <- getMugForPop()
    if(length(mug) > 0) {
      loci <- sapply(mug, FUN=function(x) x$locus)
      if (all(c("A","B") %in% loci)){ 
        probs <- calculateRaceProbability(mug)
        probablePop <- BROAD[which(probs>0.25)]
      }
    }
    
    isolate({
      updateCheckboxGroupInput(session,'population_group',selected = probablePop)
    })
  })
  observe({
    identifyPop()
  })
  
  
  # Results Header Section --------------------------------------------------
  
  output$input_mug <- DT::renderDataTable({
    
    muglist <- validateMUG()
    
    if (is.null(muglist) | length(muglist)<=1) {
      return(NULL)   
    }
    mugtable <- as.data.frame(t(formatListToTable(muglist)))
    colnames(mugtable)=LOCI
    mugtable=mugtable[-1,]
    datatable(as.data.frame(mugtable), options = list(paging = FALSE, autoWidth=TRUE,scrollX = TRUE,searching=FALSE, info = FALSE, ordering=FALSE), rownames=F)
    
  })
  
  output$data_selections <- renderUI({
    
    dataset <- names(FREQUENCY)[which(FREQUENCY==getDataset())]
    populations <- paste(getSIRE(),collapse = ', ')
    loci <- getLoci()
    
    table1 <- tags$table(class='selectionTable',
#       tags$tr(
#         tags$td(class='selectionTable',strong('Dataset: ')),
#         tags$td(dataset)
#       ),  
      tags$tr(
        tags$td(class='selectionTable',strong('Populations: ')),
        tags$td(populations)
      ),  
      tags$tr(
        tags$td(class='selectionTable',strong('Loci: ')),
        tags$td(loci)
      ))
    
    
    table1
    
  })
  
  
  # Results Haplotype Section -----------------------------------------------
  
  getHaplotypeResults <- reactive ({
    
  
    dataset <- getDataset()
    populations <- sort(getSIRE())
    loci <- convertLociOrder(getLoci())
    mug <- validateMUG()
    frequencyResults <- list()
    rowlengths <- numeric(length=length(populations))

    if (is.null(mug) | length(mug)<=1) {
      return(NULL)   
    }
    for (p in 1:length(populations)){
      thispop <- populations[p]
      results <- imputeForMug(thispop,mug,HAPLOTYPE_HOST_ADDRESS,loci=loci)
      if (results$errorStatus==FALSE){
        frequencyResults[[p]]=list(thispop,results$result)
        rowlengths[p]=nrow(results$result)
      } else {
        frequencyResults[[p]]=list(thispop,data.frame())
        rowlengths[p]=0
      }
    }
    sketch <- getHaplotypeTableContainer(populations)
    
    frequencyTable <- data.frame(matrix(NA,nrow=max(rowlengths), ncol=length(populations)*2))
    hapInds <- seq(1,length(populations)*2,2) 
    freqinds <- seq(2,length(populations)*2,2)
    
    for(p in 1:length(populations)){
      haps <- gsub('HLA-','',frequencyResults[[p]][[2]]$Haplotype)
      freqs <- format(as.numeric(frequencyResults[[p]][[2]]$Frequency), scientific = TRUE, digits=4)
      if(rowlengths[p]>0){
        frequencyTable[1:length(haps),hapInds[p]]=haps
        frequencyTable[1:length(freqs),freqinds[p]]=freqs
      }
    }
    
    return(list('table' = frequencyTable,'sketch' =sketch))
    
    
  })
  
  output$haplotypeTable <- DT::renderDataTable({
    
    results <- getHaplotypeResults()

    datatable(results$table, container = results$sketch, rownames = FALSE, 
              options = list(dom = 'ft', scrollX = TRUE, pageLength = nrow(results$table),autoWidth=TRUE, ordering=FALSE,searchHighlight = TRUE), 
              class = 'bordered')  %>%
      formatStyle(names(results$table),  backgroundColor = 'white', 'border' = '1px solid black', 'border-collapse' = 'collapse')
    

  })
  
  
  
  # Results Phased Section -----------------------------------------------
  
  getPhasedResults <- reactive ({
    
    dataset <- getDataset()
    populations <- sort(getSIRE())
    loci <- convertLociOrder(getLoci())
    mug <- validateMUG()
    frequencyResults <- list()
    TRS <- numeric(length=length(populations))
    rowlengths <- numeric(length=length(populations))

    if (is.null(mug) | length(mug)<=1) {
      return(NULL)   
    }
    for (p in 1:length(populations)){
      thispop <- populations[p]
      results <- imputePairForMug(thispop,mug,HAPLOTYPE_HOST_ADDRESS,loci=loci)
      if (results$errorStatus==FALSE){
        frequencyResults[[p]]=list(thispop,results$result)
        rowlengths[p]=nrow(results$result)
        newtrs=round(as.numeric(results$trs), digits=2)
        if(!is.null(newtrs)){
          TRS[p]=round(as.numeric(results$trs), digits=2)
        }
      } else {
        frequencyResults[[p]]=list(thispop,data.frame())
        rowlengths[p]=0
        TRS[p]=0
      }
    }
    sketch <- getPhasedTableContainer(populations)
    
    frequencyTable <- data.frame(matrix(NA,nrow=max(rowlengths), ncol=length(populations)))
    for(p in 1:length(populations)){
      hap1 <- gsub('HLA-','',frequencyResults[[p]][[2]]$Haplotype1)
      hap2 <- gsub('HLA-','',frequencyResults[[p]][[2]]$Haplotype2)
      freq1 <- frequencyResults[[p]][[2]]$Frequency1
      freq2 <- frequencyResults[[p]][[2]]$Frequency2
      freqpair <- as.numeric(frequencyResults[[p]][[2]]$'Pair Frequency')
      freqpairFormatted <-  format(freqpair,scientific = TRUE, digits=4)
      likelihood <- calculateLikelihood(freqpair)
      
      
      if(rowlengths[p]>0){
        formattedCol <- buildCellsTable(hap1, hap2, freq1, freq2, freqpairFormatted, likelihood)
        frequencyTable[1:rowlengths[p],p]=formattedCol
      }
      
    }
    
  
    TRSrow <- paste0('<p><strong> Typing Resolultion Score: </strong>',TRS,'</p>')

    frequencyTable <- rbind(TRSrow,frequencyTable)
    
    


    return(list('table' = frequencyTable,'sketch' =sketch, 'freqs'=frequencyResults))
    
  })
  
  output$phasedTable <- DT::renderDataTable({
    
    results <- getPhasedResults()
    datatable(results$table, container = results$sketch, rownames = FALSE, escape = FALSE,
              options = list(dom = 'ft', scrollX = TRUE, pageLength = nrow(results$table),autoWidth=TRUE, ordering=FALSE,searchHighlight = TRUE), 
              class = 'cell-border')  %>%
      formatStyle(names(results$table),  backgroundColor = 'white', 'border' = '1px solid black', 'border-collapse' = 'collapse')
    
  })
  
  output$phasedFreqPlot <- renderChart({
    results <- getPhasedResults()
    freqList <- results$freqs
    allfreqs <- unlist(lapply(freqList, function(x){
      sum(as.numeric(x[[2]]$'Pair Frequency'))
    }))
    pops <- unlist(lapply(freqList, function(x){
      x[[1]]
    }))
    
    data <- data.frame('Population'=pops,'Frequency'=allfreqs)
    p1 <- rPlot(x='Population', y='Frequency', data=data, type = 'bar')
    p1$guides(x = list(title = "", ticks = pops))
    p1$addParams(height = 400, dom = 'phasedFreqPlot', 
                 title = "Genotype Frequencies", xlab='')
    return(p1)
    
  })
  
  
  
  # Results Unphased Section -----------------------------------------------
  
  getUnphasedResults <- reactive ({
    dataset <- getDataset()
    populations <- sort(getSIRE())
    loci <- convertLociOrder(getLoci())
    mug <- validateMUG()
    frequencyResults <- list()
    TRS <- numeric(length=length(populations))
    rowlengths <- numeric(length=length(populations))
    
    if (is.null(mug) | length(mug)<=1) {
      return(NULL)   
    }
    for (p in 1:length(populations)){
      thispop <- populations[p]
      results <- convertToUnphased(imputePairForMug(thispop,mug,HAPLOTYPE_HOST_ADDRESS,loci=loci))
      if (results$errorStatus==FALSE){
        frequencyResults[[p]]=list(thispop,results$result)
        rowlengths[p]=nrow(results$result)
        TRS[p]=round(results$trs, digits=2)
      } else {
        frequencyResults[[p]]=list(thispop,data.frame())
        rowlengths[p]=0
        TRS[p]=0
      }
    }
    sketch <- getPhasedTableContainer(populations)
    
    frequencyTable <- data.frame(matrix(NA,nrow=max(rowlengths), ncol=length(populations)))
    for(p in 1:length(populations)){
      mug <- gsub('HLA-','',frequencyResults[[p]][[2]]$MUG)
      freqpair <- as.numeric(frequencyResults[[p]][[2]]$'Frequency')
      freqpairFormatted <-  format(freqpair,scientific = TRUE, digits=4)
      likelihood <- calculateLikelihood(freqpair)
      sortOrder <- order(freqpair, decreasing = TRUE)
      
      if(rowlengths[p]>0){
        formattedCol <- buildCellsTableMUG(mug[sortOrder], freqpairFormatted[sortOrder], likelihood[sortOrder])
        frequencyTable[1:rowlengths[p],p]=formattedCol
      }
      
    }
    
    
    TRSrow <- paste0('<p><strong> Typing Resolultion Score: </strong>',TRS,'</p>')
    
    frequencyTable <- rbind(TRSrow,frequencyTable)

    return(list('table' = frequencyTable,'sketch' =sketch, 'freqs'=frequencyResults))
    
  })
  
  output$unphasedTable <- DT::renderDataTable({
    
    results <- getUnphasedResults()
    datatable(results$table, container = results$sketch, rownames = FALSE, escape = FALSE,
              options = list(dom = 'ft', scrollX = TRUE, pageLength = nrow(results$table),autoWidth=TRUE, ordering=FALSE,searchHighlight = TRUE), 
              class = 'cell-border')  %>%
      formatStyle(names(results$table),  backgroundColor = 'white', 'border' = '1px solid black', 'border-collapse' = 'collapse')
    
  })
  
  output$unphasedFreqPlot <- renderChart({
    results <- getUnphasedResults()
    freqList <- results$freqs
    allfreqs <- unlist(lapply(freqList, function(x){
      sum(as.numeric(x[[2]]$'Frequency'))
    }))
    pops <- unlist(lapply(freqList, function(x){
      x[[1]]
    }))
    
    data <- data.frame('Population'=pops,'Frequency'=allfreqs)
    p1 <- rPlot(x='Population', y='Frequency', data=data, type = 'bar')
    p1$guides(x = list(title = "", ticks = pops))
    p1$addParams(height = 400, dom = 'unphasedFreqPlot', 
                 title = "Genotype Frequencies", xlab='')
    return(p1)
    
  })
  
  # Results Abiguities Section -----------------------------------------------
  
  getAmbiguityResults <- reactive ({
    dataset <- getDataset()
    populations <- sort(getSIRE())
    loci <- convertLociOrder(getLoci())
    if (loci=='B~C'){BC=TRUE} else {BC=FALSE}
    mug <- rollBackTyping(validateMUG(), BC)
    frequencyResults <- list()
    TRS <- numeric(length=length(populations))
    rowlengths <- numeric(length=length(populations))
    
    if (is.null(mug) | length(mug)<=1) {
      return(NULL)   
    }
    for (p in 1:length(populations)){
      thispop <- populations[p]
      results <- convertToUnphased(imputePairForMug(thispop,mug,HAPLOTYPE_HOST_ADDRESS,loci=loci))
      if (results$errorStatus==FALSE){
        frequencyResults[[p]]=list(thispop,results$result)
        rowlengths[p]=nrow(results$result)
        TRS[p]=round(results$trs, digits=2)
      } else {
        frequencyResults[[p]]=list(thispop,data.frame())
        rowlengths[p]=0
        TRS[p]=0
      }
    }
    sketch <- getPhasedTableContainer(populations)
    
    frequencyTable <- data.frame(matrix(NA,nrow=max(rowlengths), ncol=length(populations)))
    for(p in 1:length(populations)){
      mug <- gsub('HLA-','',frequencyResults[[p]][[2]]$MUG)
      freqpair <- as.numeric(frequencyResults[[p]][[2]]$'Frequency')
      freqpairFormatted <-  format(freqpair,scientific = TRUE, digits=4)
      likelihood <- calculateLikelihood(freqpair)
      sortOrder <- order(freqpair, decreasing = TRUE)
      
      if(rowlengths[p]>0){
        formattedCol <- buildCellsTableMUG(mug[sortOrder], freqpairFormatted[sortOrder], likelihood[sortOrder])
        frequencyTable[1:rowlengths[p],p]=formattedCol
      }
      
    }
    
    
    TRSrow <- paste0('<p><strong> Typing Resolultion Score: </strong>',TRS,'</p>')
    
    frequencyTable <- rbind(TRSrow,frequencyTable)
    
    
    
    
    return(list('table' = frequencyTable,'sketch' =sketch, 'freqs'=frequencyResults))
    
  })
  
  output$ambigText <- renderText({
    loci <- convertLociOrder(getLoci())
    if (loci=='B~C'){BC=TRUE} else {BC=FALSE}
    string <- "Here ambiguity is created by rolling back the patient's typing at A~B~DRB1. Typing used:"
    mug <- rollBackTyping(validateMUG(),BC )
    mugstring <- mug2string(mug)
    return(paste(string, mugstring))
  })
  
  output$ambiguityTable <- DT::renderDataTable({
    
    results <- getAmbiguityResults()
    datatable(results$table, container = results$sketch, rownames = FALSE, escape = FALSE,
              options = list(dom = 'ft', scrollX = TRUE, pageLength = nrow(results$table),autoWidth=TRUE, ordering=FALSE,searchHighlight = TRUE), 
              class = 'cell-border')  %>%
      formatStyle(names(results$table),  backgroundColor = 'white', 'border' = '1px solid black', 'border-collapse' = 'collapse')
    
  })
  
  output$ambiguityFreqPlot <- renderChart({
    results <- getAmbiguityResults()
    freqList <- results$freqs
    allfreqs <- unlist(lapply(freqList, function(x){
      sum(as.numeric(x[[2]]$'Frequency'))
    }))
    pops <- unlist(lapply(freqList, function(x){
      x[[1]]
    }))
    
    data <- data.frame('Population'=pops,'Frequency'=allfreqs)
    p1 <- rPlot(x='Population', y='Frequency', data=data, type = 'bar')
    p1$guides(x = list(title = "", ticks = pops))
    p1$addParams(height = 400, dom = 'ambiguityFreqPlot', 
                 title = "Genotype Frequencies", xlab='')
    return(p1)
    
  })
  
  
  
  # Helpers --------------------------------------------------
  
  convertToUnphased <- function(imputation){
    
    if (nrow(imputation$result)>0){
        hap1 <- strsplit(imputation$result$Haplotype1,'~')
        hap2 <- strsplit(imputation$result$Haplotype2,'~')
        recombined <- unlist(lapply(c(1:length(hap1)),function(x){
          this1 <- hap1[[x]]
          this2 <- hap2[[x]]
          
          thismug <- ''
          for (y in 1:length(this1)){
            if(y==length(this1)){connect=''} else {connect='~'}
            sorted <- sort(c(this1[y],this2[y]))
            thismug <- paste0(thismug,sorted[1],'-',sorted[2],connect)
          }
          thismug
        }))
        
        imputation$result$MUG <- recombined
        
        imputation$result=condenseMUG(imputation$result)
        
        freqs <- as.numeric(imputation$result$Frequency)
        
        imputation$trs=sum((freqs/sum(freqs))^2)
    
    }
    
    imputation
  }
  
  rollBackTyping <-function(mug, BC=FALSE){
    Loci <- c('A', 'B', 'DRB1')
    if (BC){Loci <- c('B', 'C')}
    
    newMug <- list()
    mugind <- 1
    for (l in 1:length(Loci)){
      thisLoci<-Loci[l]
      ind <- which(unlist(lapply(mug, function(x) x$locus==thisLoci)))
      if (length(ind)==1){
        thisItem <- mug[[ind]]
        if (!isSerology(thisItem$type1)){
            thisItem$type1 <- paste0(unlist(strsplit(thisItem$type1, ":"))[1],":XX")
            if (thisItem$type2 != ''){
              thisItem$type2 <- paste0(unlist(strsplit(thisItem$type2, ":"))[1],":XX")
            }
        }
        newMug[[mugind]] <- thisItem
        mugind=mugind+1
      } 
    }
    newMug
    
  }
  
  condenseMUG <- function(imp){
    
    uniMUG<- unique(imp$MUG)
    
    newdf=data.frame(matrix(nrow=length(uniMUG), ncol=2))
    
    for (h in 1:length(uniMUG)){
      thisMUG<- uniMUG[h]
      subdata <- imp[which(imp$MUG==thisMUG),]
      if(nrow(subdata)>1){
        tmp <- subdata
        subdata <- subdata[1,]
        subdata$'Pair Frequency'=sum(as.numeric(tmp$'Pair Frequency'))
      }
      newdf[h,]=subdata[,c('MUG','Pair Frequency')]
    }
    names(newdf)=c('MUG','Frequency')
    return(newdf)
  }
  
  formatListToTable <- function(mugList){
    n <- length(mugList)
    mugTable <- empty_mug
    if (n > 0){
      for (i in 1:n){
        currLocus <- mugList[[i]]$locus
        mugTable[which(mugTable$locus==currLocus),2:3] <- c(mugList[[i]]$type1, mugList[[i]]$type2)
      } 
    }
    mugTable
  }
  
  isHighResolution <- function (mug) {
    for(i in 1:length(mug)) {
      typing <- mug[[i]]
      if(!grepl(':', typing$type1) || (typing$type2 != "" && !grepl(':', typing$type2))) {
        return(FALSE)
      }
    }
    TRUE
  }
  
  rollupRace <- function(race, ethnicity){
    
    if (ethnicity == ''){
      ethnicity <- 'UK'
    }
    
    ind <- which(race==race_map[,1] & ethnicity == race_map[,2])
    
    if (length(ind)>0){
      
      return(as.character(race_map[ind,3]))
      
    }
    
    return("UNK")
  }
  
  sortMug <- function(mug){
    Loci <- c('A', 'B','BPR', 'C', 'DRB1', 'DQB1','DRB3','DRB4','DRB5')
    
    newMug <- list()
    mugind <- 1
    for (l in 1:length(Loci)){
      thisLoci<-Loci[l]
      ind <- which(unlist(lapply(mug, function(x) x$locus==thisLoci)))
      if (length(ind)==1){
        newMug[[mugind]] <- mug[[ind]]
        mugind=mugind+1
      } 
    }
    newMug
  }
  
  getBayesInput <- reactive({
    
    race_list <- BROAD
    weight <- 100/length(race_list)
    CAU <- if('CAU' %in% race_list) {weight} else {0}
    AFA <- if('AFA' %in% race_list) {weight} else {0}
    HIS <- if('HIS' %in% race_list) {weight} else {0}
    API <- if('API' %in% race_list) {weight} else {0}
    NAM <- if('NAM' %in% race_list) {weight} else {0}
    
    c(PSEUDO_COUNT = 20 , 
      CAU_PERCENT = CAU,
      AFA_PERCENT = AFA,
      HIS_PERCENT = HIS,
      API_PERCENT = API,
      NAM_PERCENT = NAM,
      THE_BIG_N = sum(c(CAU,AFA,HIS,API,NAM,5*input$constant)))
    
  })
  
  buildBayesTable <- function(mug){
    
    bayes_input <- getBayesInput()
    
    outTable <- as.data.frame(matrix(NA,nrow=5,ncol=4))
    names(outTable) <- c('Population','Prior','Likelihood','Posterior')
    outTable$Population <- c('CAU','AFA','HIS','API','NAM')
    outTable$Pseudocount <- bayes_input['PSEUDO_COUNT']
    outTable$Prior[1] <- bayes_input['CAU_PERCENT']
    outTable$Prior[2] <- bayes_input['AFA_PERCENT']
    outTable$Prior[3] <- bayes_input['HIS_PERCENT']
    outTable$Prior[4] <- bayes_input['API_PERCENT']
    outTable$Prior[5] <- bayes_input['NAM_PERCENT']
    
    for (i in 1:5){
      outTable$Prior[i] <-  (outTable$Prior[i] + bayes_input['PSEUDO_COUNT'])/bayes_input['THE_BIG_N']
    }
    bayes<-calculateBayesProbabilities(mug,race=outTable$Population,prior=outTable$Prior,HAPLOTYPE_HOST_ADDRESS)
    outTable$Posterior<-bayes$posterior
    outTable$Likelihood<-bayes$likelihood
    
    outTable
  }
  
  calculateRaceProbability <- function(mug) {
    outTable <- buildBayesTable(mug)
    outTable$Posterior
  }
  
  convertLociOrder <- function(loci){
    
    switch(loci,
           'A~C~B~DRBX~DRB1~DQB1'=loci,
           'A~C~B~DRB1~DQB1'='A~B~C~DRB1~DQB1',
           'C~B'='B~C',
           'A~C~B'='A~B~C',
           'A~B~DRB1'=loci,
           'A~C~B~DRB1'=loci
           )
    
  }
  
  getHaplotypeTableContainer <- function(populations){
    sketch = htmltools::withTags(table(
      class = '',
      thead(
        tr(
          lapply(populations, th, colspan = 2,class='populationHeader bordered')
        ),
        tr(
          lapply(rep(c('Haplotype', 'Frequency'), length(populations)), th, class='bordered')
        )
      )
    ))
  }
  
  getPhasedTableContainer <- function(populations){
    sketch = htmltools::withTags(table(
      class = '',
      thead(
        tr(
          lapply(populations, th,class='populationHeader bordered')
        )
      )
    ))

  }
  
  calculateLikelihood <- function(freqs){
    freqs <- as.numeric(freqs)
    if (length(freqs)>0){
        total <- sum(freqs)
        return(round((freqs/total)*100, digits=2))
    } else {
      0
    }
  }
  
  buildCellsTable <- function(hap1, hap2, freq1, freq2, freqpair, likelihood){
    
    count <- length(hap1)
    resultcol <- data.frame(matrix(NA,ncol=1,nrow=count))
    for (r in 1:count){
      
      cell1=paste('<table class="innertable">',
          '<tr>',
              '<td><strong>Haplotype</strong></td>',
              '<td>',gsub('~','<br>',hap1[r]),'</td>',
                #'<td>',hap1[r],'</td>',
               '<td>',gsub('~','<br>',hap2[r]),'</td>',
               #'<td>',hap2[r],'</td>',
          '</tr>',
          '<tr>', 
              '<td><strong>Frequency</strong></td>',
              '<td>',format(freq1[r], scientific = TRUE,digits = 4),'</td>',
              '<td>',format(freq2[r], scientific = TRUE,digits = 4),'</td>',
          '</tr>',
          '<tr>', 
          '<td><strong>Genotype<br>Frequency</strong></td>',
          '<td>',format(freqpair[r], scientific = TRUE,digits = 4),'</td>',
          '<td></td>',
          '</tr>',
          '<tr>', 
          '<td><strong>Likelihood</strong></td>',
          '<td>',paste0(round(likelihood[r],digits=3),'%'),'</td>',
          '<td></td>',
          '</tr>',
          '</table>'
          )
      
      
      resultcol[r,]=cell1
      
      
    }
    
    resultcol
  }

  buildCellsTableMUG <- function(MUG, freqpair, likelihood){
    
    count <- length(MUG)
    resultcol <- data.frame(matrix(NA,ncol=1,nrow=count))
    for (r in 1:count){
      
      cell1=paste('<table class="innertable">',
                  '<tr>',
                  '<td><strong>Unphased<br>Genotype</strong></td>',
                  '<td>',gsub('~','<br>',MUG[r]),'</td>',
                  '</tr>',
                  '<tr>', 
                  '<td><strong>Frequency</strong></td>',
                  '<td>',format(freqpair[r], scientific = TRUE,digits = 4),'</td>',
                  '</tr>',
                  '<tr>', 
                  '<td><strong>Likelihood</strong></td>',
                  '<td>',paste0(round(likelihood[r],digits=3),'%'),'</td>',
                  '</tr>',
                  '</table>'
      )
      
      
      resultcol[r,]=cell1
      
      
    }

    
    resultcol
  }
  
  isSerology <- function(typing){
    first <- substr(typing,1,1)
    hasColon <- grepl(':',typing)
    if (first == 's' && !hasColon){
      return (TRUE)
    } 
    
    FALSE
  }
  
  mug2string <- function(mug){
    
    full <- ''
    
    for (loci in 1:length(mug)){
      
      this <- mug[[loci]]
      twoCopyCheck <- this$type2!=""
      
      if(grepl(':',this$type1)){  
        a <- paste0(this$locus,"*",this$type1)
        
        if (twoCopyCheck){
          a <- paste0(a,'/',this$locus,"*",this$type2)
        }
        
      } else { ### serology
        
        a <- paste0(this$locus,sub('s','',this$type1))
        
        if (twoCopyCheck){
          a <- paste0(a,'/',this$locus,sub('s','',this$type2))
        }
      }
      
      if (loci<length(mug)){
        full <- paste0(full,a,', ')
      } else {
        full <- paste0(full,a)
      }
      
    }
    
    full
    
  }
  
  
})