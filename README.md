# Semantic halo of meter (Russian corpus)

## Meter &amp; meaning relationships in poetry  


- To follow main analysis pipeline use **00_main_analysis.ipynb** (everything is ready for that);  
  
However, if you want to retrace preparation and model building steps:  
  
- To check the Document-Term matrix and cleaning procedures, use prepare_matrix.ipynb. **NB.** DTM in **data/** folder should be unzipped before it!;  
- Use cleaned output to build LDA model with **topicmodels** package in **build_lda.ipynb**;  
- Then return to main_analysis notebook. All intermediate results (clean matrix & essential LDA outputs) were saved in **data/** folder as .Rdata objects;  
- All important functions are in **src/**  
