CCTransformationExtraction is a program to extract concept transformations from geo-analytical questions. 

## Installation
### Requirements
The program is implemented in python (version 3.7). Several libraries should be installed (use pip command to install the following libraries) before running the code:
```
pip install numpy
pip install allennlp
pip install -U spacy
pip install nltk
pip install word2number
```

## Usage
Run the script `Identify.py` on `GeoAnQu.txt` to get training results `Results_train_Auto.json`.
Check the `Grammar` folder for more details of the functional grammar in `GeoAnQu.g4`.
Check the `Dictionary` folder for the concept dictionary.

To check the evaluation, navigate to `CCTrans_evaluation` folder in the project folder, and use `evalTest.py` to generate `EvaluationResults.csv`.
`Results_test_Auto.json` is generated by running `Identify.py` on `test corpus.txt`.
`Results_test_Manual.json` is manually generated as the gold standard used in evaluation.


## License
[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/)