dashboardPage(
  dashboardHeader(title='Haplostats'),
  dashboardSidebar(
    includeCSS("www/custom.css"),
    sidebarUserPanel(htmlOutput('username')),
    sidebarMenu(id='mainMenu',
                menuItem("Instructions", tabName = "instructionstab"),
                menuItem("Input", tabName = "inputtab"),
                menuItem("Results", tabName = "resultstab")
    )
  ),
  dashboardBody(
    img(src="logo2.png",alt='logo2.png', height=100),
    tabItems(
      tabItem(tabName = "instructionstab",   
              h1('Welcome to Haplostats!'),
              br(),
              h3('How to use Haplostats:'),
              a('Transplant Center User', href='https://bioinformatics.bethematchclinical.org/workarea/downloadasset.aspx?id=10957', target="_blank"),
              br(),
              a('HLA Researcher', href='https://bioinformatics.bethematchclinical.org/workarea/downloadasset.aspx?id=6339', target="_blank"),
              br(),br(),hr(),
              p(strong('DISCLAIMER: The data available here are intended for research purposes only.'))
      ),
      tabItem(tabName = "inputtab",
              br(),
              fluidRow(
                box(width = 4,      
                    title = 'ID Input',
                    p(textInput('ID', label='ID or GUID')),
                    tags$style(type="text/css", '#ID {width: 300px;}'),
                    uiOutput('error_box'),
                    selectInput('id_type', label='ID Type',
                                 choices=list('Donor ID'='donor',
                                              'Patient ID'='patient',
                                              'Cord ID'= 'cord',
                                              'Guid'= 'guid'),width = '300px'),
                    actionButton('ID_lookup','ID Lookup'),
                    hr(),
#                     selectInput('loci', label = 'Haplotype Loci', 
#                                 choices = c('x/10: A/C/B/DRB1/DQB1'='A~C~B~DRB1~DQB1',
#                                             'x/8: A/C/B/DRB1'='A~C~B~DRB1'),
#                                 selected='A~C~B~DRB1~DQB1',width = '300px')
                    selectInput('loci', label = 'Haplotype Loci', 
                                choices = c('x/10: A/C/B/DRB1/DQB1'='A~C~B~DRB1~DQB1',
                                            'x/8: A/C/B/DRB1'='A~C~B~DRB1',
                                            'C~B',
                                            'A~C~B',
                                            'A~B~DRB1',
                                            'A~C~B~DRB1'),
                                selected='A~C~B~DRB1~DQB1',width = '300px')
#                     hr(),
#                     selectInput('dataset', label = 'HLA Dataset', 
#                                 choices = c('NMDP high res 2007'='data2007', 'NMDP full 2011'='data2011'), 
#                                 selected='data2011',width = '300px')
                    
                    
                ),
                box(width = 6,   
                    title='HLA Typing',
                    p(strong('HLA Table Input'),br(), 'Example: 01:02 or 07:CGNF'),
                    actionLink('fillExample', 'See example typing.'),
                    uiOutput('table_mug'),
                    
                    uiOutput('mug_invalid'),
                    br(),
                    actionButton('resetTyping','Clear All Typings')
                    
                )
              ),
              fluidRow(
                box(width = 10,   
                    title='Populations',
                    div(style="display:inline-table;margin:30px;",
                    checkboxGroupInput('population_group',label="Broad Races",
                                       choices=list('African American (AFA)'='AFA',
                                                    'Asian Pacific Islander (API)'='API',
                                                    'White (CAU)'='CAU',
                                                    'Hispanic (HIS)'='HIS',
                                                    'Native American (NAM)'='NAM'), selected=c('AFA','API','CAU','HIS','NAM'))
                    ),
                    div(style="display:inline-table;margin:30px;",
                        checkboxGroupInput('AFApopulation_group',label="AFA Detailed",
                                           choices=list('AAFA','AFB','CARB','SCAMB'))),
                    div(style="display:inline-table;margin:30px;",
                        checkboxGroupInput('APIpopulation_group',label="API Detailed",
                                           choices=list('AINDI','FILII','HAWI','JAPI', 'KORI','NCHI','SCSEAI', 'VIET'))),
                    div(style="display:inline-table;margin:30px;",
                        checkboxGroupInput('CAUpopulation_group',label="CAU Detailed",
                                           choices=list('MENAFC','NAMER'))),
                    div(style="display:inline-table;margin:30px;",
                        checkboxGroupInput('HISpopulation_group',label="HIS Detailed",
                                           choices=list('CARHIS','MSWHIS','SCAHIS'))),
                    div(style="display:inline-table;margin:30px;",
                        checkboxGroupInput('NAMpopulation_group',label="NAM Detailed",
                                           choices=list('AISC','ALANAM','AMIND','CARIBI'))),
                    br(),
                    actionButton('selectAllPop','Select All Populations'),
                    actionButton('identifyPop','Identify Populations'),
                    actionButton('resetPop','Clear All Populations')
                    
                )
              ),
              actionButton('resetPageFull','Clear All Fields'),
              actionButton('submit_query','Submit Query')
      ),
      tabItem(tabName = "resultstab",  
              br(),
              br(),
              box(width=12,height = 170,
                  column(width=4, id='data_selections_col', br(), htmlOutput('data_selections')),
                  column(width=8, id= 'typings_col', DT::dataTableOutput('input_mug'))
              ),
              br(),
              br(),
              
              tabBox(width=12,selected = 3,
                tabPanel("Haplotypes", h1('Haplotypes'),value = 1,
                       DT::dataTableOutput('haplotypeTable', width = '100%')),
                tabPanel("Phased Genotypes", h1('Phased Genotypes'),value=2,
                         showOutput('phasedFreqPlot', "polycharts"),
                         DT::dataTableOutput('phasedTable', width = '100%')),
                tabPanel("Unphased Genotypes", h1('Unphased Genotypes'),value=3,
                         showOutput('unphasedFreqPlot', "polycharts"),
                         DT::dataTableOutput('unphasedTable', width = '100%')),
                tabPanel("Ambiguities", h1('Ambiguity'),textOutput('ambigText'),value=4,
                         showOutput('ambiguityFreqPlot', "polycharts"),
                         DT::dataTableOutput('ambiguityTable', width = '100%'))
              )
              
              
      )
    )
  )
)