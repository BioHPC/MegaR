megaR: An Interactive R Package for Metagenomic Sample Classification and Disease Prediction using Microbiota and Machine Learning
-----------------------------------------------------------------------

Click the wiki link for details:
<https://github.com/BioHPC/megaR/wiki/megaR:-An-Interactive-R-Package-for-Metagenomic-Sample-Classification-and-Disease-Prediction-using-Microbiota-and-Machine-Learning>

Welcome to the megaR wiki!

An user-friendly interactive machine learning interface for metagenomic analysis to identify and predict disease sample accurately.
----
**Pre-requisites:**

* R version
    * Download R (>3.4.0) version from CRAN.
    * Windows: https://cran.r-project.org/bin/windows/base/
    * Mac OS X: https://cran.r-project.org/bin/macosx/
    * Linux: https://cran.r-project.org/bin/linux/

* Libraries
    * shiny; shinythemes; shinydashboard; randomForest; stringr; caret
    * plyr; ggplot2; RColorBrewer; biomformat
----
Machine learning has been a very interesting field which has been utilized in many fields from biomedical imaging to business analytics. Machine learning is stipulated to be a strong tool for diagnostics and even for determining therapeutics in future as we move to personalized medicine. 

MegaR provides an unprecedented opportunity to develop  machine learning models from metagenomic data available publicly as well as to perform do classification of data based on the optimal model we developed. 

The general workflow is described in below.


![](https://github.com/BioHPC/megaR/blob/master/screenshot/Interface.png)

**Data Input**

megaR can take both otu table and biom file from popular metagenomic profiling tools, metaphlan and qiime. 
After the data is uploaded, the contents of the data is displayed under the data tab.


**Data Preprocessing**

In order to preprocess the data, click preprocess tab and select appropriate taxonomic level information to use for machine learning. The slider bar can be adjusted to select the percentage of sample that should include the threshold amount of presence in the data. Finally there is a choice for normalizing the data. After choosing either to normalize the data or not, the processed data that is ready for building machine learning models is seen under data tab.

![](https://github.com/BioHPC/megaR/blob/master/screenshot/Preprocessing.png)

**Model Development**

Select appropriate machine learning model for classification. Currently only Random Forest models is available. 

Upload the metadata that contain information about the sample.The metadata should be tab separated files with rows containing sample ids and columns containing the other information like class that each sample belongs to. The sample id in the metadata must match exactly to sample id in initial metaphlan/qiime files. Mention which column in metadata contain id that match with the initial metaphlan and qiime result and also the column where the class this sample are stored is mentioned. Then select the percentage of data that you want to use to train a model. One can use as much as 100% of the data but then there will be no test set to generate confusion matrix. The error rate of prediction during training an model is given by the plot under error rate. 

![](https://github.com/BioHPC/megaR/blob/master/screenshot/Test_error_stat.png)
![](https://github.com/BioHPC/megaR/blob/master/screenshot/Train_error_plot.png)

The error rate of prediction on test set is a better estimate of model accuracy and it can be estimated using confusion matrix generated under confusion Matrix tab.

![](https://github.com/BioHPC/megaR/blob/master/screenshots/test_error)

From a practical perspective, it is important to identify features that are important in identifying the class of metagenomic sample. The top ten important species or genus crucial in identifying the class of sample along with their variable importance is shown under the **Important Feature** tab.

![](https://github.com/BioHPC/megaR/blob/master/screenshot/Top_ten_impt-feature.png)

An additional feature of tool that can improve accuracy is **class to remove**  tab. When more than two class are present, a class can be removed to increase accuracy. This also allows to remove control from the class category.

**Prediction**

Finally, we can upload unknown test set and get the prediction on which category they fall into as a list. Unknown set must be biom file if model is developed from qiime data and merged metaphlan table if model is developed from the metaphlan data.

![](https://github.com/BioHPC/megaR/blob/master/screenshots/Prediction.png)
