# Creating a Shiny App from a list of URL links enclosed in a PDF file

## Live app

https://rafalopezv.shinyapps.io/pdf_to_shiny/

## Folder structure

1. limpieza_links.R cleans the data contained at the input folder and export it to folder output
1. app.R pulls the data from the output folder and creates de app

## General Procedure

1.  [This book](https://rafalopezv.io/static/qog/libro_roger.pdf) (pages 330 and 347) lists a collection of more than 200 newspaper article links classified into topics.

2.  I copied all the links and asked Chatgpt (v. 3.5) to break new lines every time it finds the pattern "http". The result was copied into a spreadsheet editor. 

3.  All wrangling was done in R with some assistance from Chatgpt, mainly to speed up the whole process.

4.  `rvest` library was used to extract publication dates and titles directly from each link. 

5. R created Bootstrap cards thanks to the Shiny syntax. Cards and general information were merged into a data frame and used as the main source in Shiny. 

## What Chatgpt Did?

1. It broke messed text with URLs into text with line breaks so I could copy the information into a database like Excel. I used Numbers. 

2. Helped translate Bootstrap code into Shiny syntax. The prompt was: "Convert this HTML code into Shiny format." Shiny is a library for the R programming language. 

3. Cleaned more than \~200 dates irregularly written into well-formatted date entries. The prompt was: "These strings of dates need to be translated into well-formatted data of this form: 'YYYY-MM-DD' where Y stands for year of four digits, M for months of two digits, and D for days of two digits." 

## Where will this Shiny eventually belong?

[This link](https://rogercortez.netlify.app/) will contain the app. When that happens, code comments in English will be in Spanish and all UI language will change too.

This code is merely organized as a code demonstration.
