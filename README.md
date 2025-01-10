# Modular_ML_functions
This repo contains a set of ML functions to assess performance of different combinations of cluster-assigned features.

The purpose of these functions is to conduct an exploratory analysis to identify an optimal machine learning algorithm for the user's data, and to fine-tune the feature space to use in the identified algorithm via a combinatorix exercise. Additionally, there is functionality to create an ensemble model from these functions and feed the outputs into a specific ensemble model function here.


# Running the model search functions
To execute these functions, users will require the following:
1) CARET and progressr libraries installed (caretEnsemble if the ensemble function will be used)
2) Training and validation data matrices
3) Training and validation metadata tables with the actual classification assignments as factors
   a) For the svm and ML_suite functions, an id_column will be required to combine the data matrices and metadata tables since caret's svm wrapper requires formulaic input
4) A data.frame of the complete feature space and their cluster assignments, known as modules
   a) Clusters can be self-assigned based on self-defined heuristics, or assigned via clustering algorithms such as hierarchical clustering
5) A vector of strings containing all desired module combinations to test, with modules within each combination seperated by an underscore (i.e. "_")

# Optinal components for running the model search functions
While the functions are set to run on their own, users have the option to tune the model design and outputs via the following:
1) Caret's trainControl object can be included by the user should they want to define different paramaters than the function-defined default (i.e. folds, repeats, method, etc.)
2) The metric used by the train function, as well as whether to maximize or minimize, are definable.
   a) **NOTE:** Caret's twoClassSummary and multiClassSummary are used here, and so the selected metrics must align with either of these

# Function outputs
All functions will output their generated model and confusion matrix objects for further assessment. Additionally, all (except the ML_suite and ensemble functions) will print performance statistics for each combination tested as the function runs so users can view them in real-time.
