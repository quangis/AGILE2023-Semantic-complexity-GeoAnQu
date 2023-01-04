# AGILE2023-Semantic-complexity-GeoAnQu

Python/R source codes and datasets used for the submission to AGILE2023 conference


## Content structure

- "main.py": run this Python code to apply the transformation parser on the five corpora. See the section below on how to run this code.
- "geo_question_parser_haiqi": this folder contains the transformation parser library.
- "inputCorpora": this folder contains the five corpora.
    - "\[corpus name\[.txt": a table listing all questions of the corpora. The column "Question" lists the original questions in the corpora. The column "RQuestion" lists the revised questions on which the transformation parser is applied.
    - "\[corpus name\]\_missing.json": manually created transformations that were not correctly parsed by the transformation parser
- "outputData": all output from "main.py" is stored inside this folder.
    - "\[corpus name\[.json": contains concepts ("types" json object) and transformations ("transformations" json object) for each question in the corpus.
    - "\[corpus name\[\_ParserStats_r.json": contains descriptive statistics about concepts and transformations of each question
        - "Question": question string
        - "qTypesCount": number of concepts identified in the question (size of "types" json object)
        - "qTransCount": number of transformations identified in the question (size of "transformations" json object)
        - "qOutputType": goal concept of the question
        - "\[concept name\]": 1 if the concept appears in the question or 0 otherwise
- "statsScript.R": R code for statistical analysis of the output inside the folder "outputData". See the section below on how to run this code.
    

## Running the Python code "main.py"

#### Setting up the conda python environment:
    
1.Install the 64-Bit version of Miniconda 4.10.3  from (https://repo.anaconda.com/miniconda/) (Windows,MaxOS and Linux).

2.Open a new window of anaconda prompt. Create a new conda environment with a name “*qparser*”.

    conda create -n qparser python=3.9.7

3.Activate the new environment:

    conda activate qparser

4.Install *allennlp* package

    pip install allennlp 

5.Install *allennlp-models* package

    pip install allennlp-models

6.Install *spacy* package from conda-forge

    conda install -c conda-forge spacy

7.Install *spacy* trained pipeline. If the installation throws error, then try executing the command again.

    python -m spacy download en_core_web_sm

8.Install other packages from conda-forge:

    conda install -c conda-forge antlr4-python3-runtime=4.9.3 word2number pyzmq nltk 

9.Install *nltk* modules:

    python -m nltk.downloader averaged_perceptron_tagger
    
    python -m nltk.downloader omw-1.4

10.Optionally, it may be necessary to install the checklist package:
    
    pip install checklist


#### Running "main.py"

In the anaconda prompt, perform following steps:

1.Navigate to the folder containing "main.py"

2.Execute "main.py" with the "python" command.


## Running the R code "statsScript.R"

It is recommended to execute the R code with the latest version of RStudio running on R version 4.2.2. 

Before executing "statsScript.R", make sure to assign the variable "source" within the R code to the path of folder containing "statsScript.R". 

All necessary packages will be automatically install upon the first execution of "statsScript.R".

Analysis results will be printed in the console of RStudio and visualized as plots.
