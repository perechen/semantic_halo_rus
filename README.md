## Code & Data for "Weak Genres" etc.

In general workflow starts from **00_preparations_exploration** - this script already works with pre-trained LDA model.  

However, you can follow all the steps of document-term matrix preparation (cleaning meter labels, filtering years,etc) from scratch: **prepare_matrix** -> **build_lda**  

You can also start from any **0x** notebook since LDA output data is included in the repository from the start.

Jupyter notebooks [are used with R kernel](https://irkernel.github.io/installation/), to add kernel to Jupyter, run:

```
install.packages('IRkernel') # installs kernel

IRkernel:installspec()
```

### Structure

#### data/

- **dtm_fin.Rda**: holds final Document-Term matrix with word counts, after all preprocessing steps.  

- **dtm_vanilla_fin.Rda**: DTM without contextual word replacements.

- **lda_output.rda**: derived data from 80 topics LDA model. Main source of all experiments.  

- **ru_lda.Rda**: unprocessed LDA model;

- **TD_matrix_tf_years_5000.tsv.zip** basic matrix before most of preprocessing and filtering steps

#### src/

Holds scripts for iterative experiments and preprocessing steps;

#### main scripts

- **00_preparations_exploration**: given an LDA model input, prepares necessary data (topics, topic probablities in documents, labels, etc.)

- **01_similarities_within**: given derived LDA data (**lda_output.rda**) calculates consensus trees of within-meter clustering. Tests general clustering accuracy across different sample sizes.  

- **02_similarities_between**: given derived LDA data (**lda_output.rda**) calculates consensus tree for between-meter family clustering, tests general accuracy on the controlled dataset.

- **03_chronology**: given derived LDA data (**lda_output.rda**), calculates clustering accuracy across time periods

- **04_semantic_inequalities**: given derived LDA data (**lda_output.rda**), calculates Gini coefficients of topic expression in meters against a randomly redistributed poems.

#### data preparation scripts

- **prepare_matrix**: takes an original matrix and prepares it for modeling: cleans meter labels, subsets data by year and poem size.

- **build_lda**: given the final dtm (**dtm_fin.Rda**), build an LDA topic model
