## Dan Smilowitz
## DATA 608 Assignment 3
## Question 2

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Improvement in Mortality Rate by State vs. National Average, 1999-2010"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Cause", "Cause of Death:",
                  list("Certain infectious and parasitic diseases" = "A00-B99",
                       "Neoplasms" = "C00-D48",
                       "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" = "D50-D89",
                       "Endocrine, nutritional and metabolic diseases" = "E00-E88",
                       "Mental and behavioural disorders" = "F01-F99",
                       "Diseases of the nervous system" = "G00-G98",
                       "Diseases of the circulatory system" = "I00-I99",
                       "Diseases of the respiratory system" = "J00-J98",
                       "Diseases of the digestive system" = "K00-K92",
                       "Diseases of the skin and subcutaneous tissue" = "L00-L98",
                       "Diseases of the musculoskeletal system and connective tissue" = "M00-M99",
                       "Diseases of the genitourinary system" = "N00-N98",
                       "Pregnancy, childbirth and the puerperium" = "O00-O99",
                       "Certain conditions originating in the perinatal period" = "P00-P96",
                       "Congenital malformations, deformations and chromosomal abnormalities" = "Q00-Q99",
                       "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R00-R99",
                       "External causes of morbidity and mortality" = "V01-Y89")
      )
    ),
    
    mainPanel(
      htmlOutput("gvisplot")
    )
  )
))
