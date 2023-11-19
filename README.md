# (Computational) Text Analysis 1

This repository includes learning materials for students attending the workshop "(Computational) Text Analysis 1" at the University of Sheffield. The aim of this workshop is to provide a gentle introduction to computational text analysis using the R programming language. To be able to follow the workshop, you will need to have R and RStudio installed on your computer. For a quick guide on how to install the software, you can refer to this [video](https://www.sheffield.ac.uk/mash/stats-resources/r#) by MASH (Maths and Statistics Help) at UoS. 



During this workshop (and also during "Text Analysis 2") you will be using a dataset of academic journal article abstracts ranging from 2000-2023 and published in the _Housing Studies_ journal. A link to download the dataset will be provided during the workshop. This dataset has been compiled by Michael Marshall, a PGR student at the Department of Urban Studies and Planning at the University of Sheffield. Please do not use this dataset for your own projects, and do not circulate it with other students.

The dataset was extracted using the Scopus API. It has 27 variables, most of which are quite self-explanatory (e.g., abstract, author, keywords, document type...). Three of the variables that have been generated by Michael and his colleagues and require a bit of additional context:

- united_kingdom - binary variable for whether corresponding author is based in United Kingdom
- united_states - binary variable for whether corresponding author is based in United States
- quant - this is a binary variable for whether the paper uses quantitative methods
