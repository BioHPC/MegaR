A user-friendly interactive machine learning interface for metagenomic analysis to identify and predict disease sample accurately.
----
**Pre-requisites:**

* R version
    * Download R (>3.6.0) version from CRAN.
    * Windows: https://cran.r-project.org/bin/windows/base/
    * Mac OS X: https://cran.r-project.org/bin/macosx/
    * Linux: https://cran.r-project.org/bin/linux/
    
 * Libraries:
   * devtools

To install devtools, use below command:

> install.packages("devtools") 

Note: megaR also uses shiny, shinythemes; randomForest; stringr; caret, plyr; ggplot2; RColorBrewer; biomformat; biomaRt. However, those packages will be installed if using install_github from below.

***Installing megaR package:***

Using an R interface, type:

> devtools::install_github("BioHPC/megaR") 

**Introduction**
  
Machine learning is a very interesting field which has been utilized in many fields from biomedical imaging to business analytics. Machine learning is stipulated to be a strong tool for diagnostics and even for determining therapeutics in future as we move to personalized medicine. 

MegaR provides an unprecedented opportunity to develop  machine learning models from metagenomic data available publicly as well as to perform do classification of data based on the optimal model we developed. 

The description below walk you through the analysis of the WGS of three country cohort where samples are categorized based on their country of origin using machine leanring techniques. The general workflow is described in below.

**Graphical Interface Workflow** 

library(megaR)

megaR() 

**Data Input**

megaR can take both otu table and biom file from popular metagenomic profiling tools, [metaphlan](https://www.nature.com/articles/nmeth.2066) and [qiime](https://www.nature.com/articles/nmeth.f.303).
The data set can be found inside data folder. Clicking the browse tab, user can upload the input file from any where in the computer path.  After the data is uploaded, the contents of the data is displayed under the data tab. 

![](https://github.com/BioHPC/megaR/blob/master/screenshot/data_input.png)

   **Data Preprocessing**

After the data is loaded, it can be preprocessed to allow eficient machine leaning. Click preprocess tab and select appropriate taxonomic level information to use for machine learning. Genus Level and Species Level tab returns genuses and species level from the dataset as the feature. All tabs trakes back the taxon level for unclassified higher order. The slider bar can be adjusted to select the percentage of sample that should include the threshold amount of abundance present in the data. Finally there is a choice for normalizing the data. After choosing either to normalize the data or not, the processed data that is ready for building machine learning models is seen under data tab.

![](https://github.com/BioHPC/megaR/blob/master/screenshot/Preprocess_genus.png)

   **Model Development**

There are three machine learing model available for classification purpose. Select appropriate machine learning model.

Upload the metadata that contain information about the sample.The metadata should be tab separated files with rows containing sample ids and columns containing the other information like class that each sample belongs to. The sample id in the metadata must match exactly to sample id in initial metaphlan/qiime files. Mention which column in metadata contain id that match with the initial metaphlan and qiime result and also the column where the class this sample are stored is mentioned. Then select the percentage of data that you want to use to train a model. One can use as much as 100% of the data but then there will be no test set to generate confusion matrix. The error rate of prediction during training an model is given by the plot under error rate.


![](https://github.com/BioHPC/megaR/blob/master/screenshot/rf_plot_train.png)
![](https://github.com/BioHPC/megaR/blob/master/screenshot/test_error_stats.png)

An additional feature of tool that can improve accuracy is **select level to classify**  tab. When more than two classes are present, only the class that are examined for classification can be selected. This also allows to remove control and other less important class from the model thus increasing the model accuracy.

The plot can be downloaded by clicking in download the plot button to save for future use.

The error rate of prediction on test set is a better estimate of model accuracy and it can be estimated using confusion matrix generated under confusion Matrix tab. The Statistics of how well the model performs can also be obtained using the statistics tab.

![](https://github.com/BioHPC/megaR/blob/master/screenshot/test_error.png)

From a practical perspective, it is important to identify features that are important in identifying the class of metagenomic sample. The top ten important species or genus crucial in identifying the class of sample along with their variable importance is shown under the **Important Feature** tab.
 
   **Validation**
 ![](https://github.com/BioHPC/megaR/blob/master/screenshot/validation.png)
 
   **Prediction**

Finally, we can upload unknown test set and get the prediction on which category they fall into as a list. Unknown set must be biom file if model is developed from qiime data and merged metaphlan table if model is developed from the metaphlan data.

![](https://github.com/BioHPC/megaR/blob/master/screenshot/Prediction.png)
