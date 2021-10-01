# MegaR: A user-friendly interactive machine learning interface for metagenomic analysis to identify and predict disease sample accurately.

### Cite this article
Dhungel, E., Mreyoud, Y., Gwak, HJ. et al. MegaR: an interactive R package for rapid sample classification and phenotype prediction using metagenome profiles and machine learning. BMC Bioinformatics 22, 25 (2021). [https://doi.org/10.1186/s12859-020-03933-4](https://doi.org/10.1186/s12859-020-03933-4)

### Summary
  
Machine learning has been utilized in many applications from biomedical imaging to business analytics. Machine learning is stipulated to be a strong method for diagnostics and even for determining therapeutics in future as we move to personalized medicine. MegaR provides an unprecedented opportunity to develop machine learning models from metagenomic data available publicly as well as to perform classification of data samples based on the optimal model we developed. 

The description below walks you through the analysis of the WGS of T1D cohort from DIABIMMUNE project (https://pubs.broadinstitute.org/diabimmune) where the goal of this cohort is to compare microbiome in infants who have developed type 1 diabetes (T1D) or serum autoantibodies (markers predicting the onset of T1D) with healthy controls in the same area.

The general workflow is described in below.

If you would like to preview MegaR without downloading the package, you can visit https://megar.shinyapps.io/preview 
*Note:* You will still need to download the data set locally. That can be done [here](https://github.com/BioHPC/MegaR/tree/master/data/WGS/T1D.zip).


### Pre-requisites:

* R version
    * Download R (>3.6.0) version from CRAN.
    * Windows: https://cran.r-project.org/bin/windows/base/
    * Mac OS X: https://cran.r-project.org/bin/macosx/
    * Linux: https://cran.r-project.org/bin/linux/

## Installing MegaR:

There are two ways to install MegaR. The first is using devtools, and the second is using the MegaR.tar.gz file.

#### [1] Devtools installation:

 * Libraries:
   * devtools

To install devtools, use below command:
```  
> install.packages("devtools") 
```  
*Note*: MegaR also uses shiny, shinythemes; randomForest; stringr; caret, plyr; ggplot2; RColorBrewer, DT. However, those packages will be automatically installed if using install_github from below.


Using an R interface, type:
```  
> devtools::install_github("BioHPC/MegaR") 
```  

#### [2] MegaR.tar.gz installation:

Download the MegaR.tar.gz file from [here](https://github.com/BioHPC/MegaR/blob/master/MegaR_1.0.tar.gz)

In the R console, type:
```
>setwdir("/path/")
```
Where path is the location of the downloaded MegaR.tar.gz file.

Once in the correct location, type:
```
>install.packages(MegaR.tar.gz)
```
Alternatively, you may install the package using the R interface by going to:
```
Tools > Installpackages... > Package Archive File > Browse... > MegaR.tar.gz
```

## Getting Started

In RStudio, use following command:

```
> library(MegaR)
> MegaR() 
```

### Data Input ###

MegaR can take both OTU table and BIOM file from popular metagenomic profiling tools, [metaphlan](https://www.nature.com/articles/nmeth.2066) and [qiime](https://www.nature.com/articles/nmeth.f.303).
MegaR provides sample data from the DIABIMMUNE project. If you clone or download the full MegaR package, the data files are located in data folder. Otherwise, the data set used for this example is the T1D dataset that can be downloaded from [here](https://github.com/BioHPC/MegaR/tree/master/data/WGS/T1D.zip). 

* Clicking the browse tab, user can upload the input file from anywhere in the computer path.
* For T1D experiment, upload **merged_metaphlan_t1d.txt** file.
* After the data is uploaded, the contents of the data are displayed as an interactive table under the **Data** tab. 

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/input.gif)

### Data Preprocessing ###
After the data is loaded, it can be preprocessed to allow efficient machine leaning. Click preprocess tab and select the appropriate taxonomic level information to use for machine learning.

#### Criteria for feature selection ####
**Genus Level** and **Species Level** tabs return genus and species level from the dataset as the feature. **All Level** tab tracks back the taxon level for unclassified higher order. For the T1D experiment, 

* Clicking **Species Level** for feature selection.

#### Threshold ####
This field is getting a floating number to remove profiles and their abundances below the threshold value. Default value is **0.003**. 

* For the T1D experiment, just leave the the deafult value (0.03).

#### Percentage of Sample #### 
The **Percentage of Sample** slider bar can be adjusted to select the percentage of sample that should include the threshold amount of abundance present in the data. Default value is **5%**. 

* For the T1D experiment, just leave the the deafult value (5%).

#### Normalization ####
There is a choice for normalizing the data. MegaR provides fours choices for data normalization: **Cumulative Sum Scaling (CSS)**, **Quantile**, **Trimmed Mean of M-values (TMM)**, and **NO**. After choosing the desired normalization, the processed data that is ready for building machine learning models is seen under the data tab. 

* For the T1D experiment, select **NO**. 

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/preprocessing.gif)

### Model Development ###

There are three machine learning models available for classification. **Generalized Linear Model (GLM)**, **Random Forest Model**, and **Support Vector Machines**. If a user click the **Model building** tabl, the user can select the appropriate machine learning model. 

* For the T1D experiment, select **Random Forest**. 

#### Upload a metadata file ####
Upload a metadata file containing information about the sample dataset. The metadata should be tab-separated file with rows containing sample ids and columns containing the other information like class that each sample belongs to. The sample ID in the metadata **must match** exactly to sample ID in the initial metaphlan/qiime file. 

* For the T1D experiment, upload **metadata.tsv** file in the T1D dataset.

#### Column number for class info ####
Provide a column number for class info. For the T1D expeirment, column 7 (T1D_Diagnosed) has the class values (t, f) where t is True and f is False.

* For the T1D experiment, provide **7** for this field.

#### Column number for sample ID ####
Provide a column number for sample IDs. For the T1D expeirment, column 2 (Gid_shotgun) has the sample IDs. 

* For the T1D experiment, provide **2** for this field.

#### Percentage of data in training ####
Then select the percentage of data that you want to use to train a model. One can use as much as 100% of the data but then there will be no test set to generate confusion matrix. 

* For the T1D experiment, provide **80** for this field.

#### Select classification labels ####
Provide lables of the classification. For example, the T1D metadata column 2 has the class values (t, f) where t is True and f is False. MegaR automatically pulls the class labels from the provided column number for the class info.

* For the T1D experiment, select **f** and **t** in this field.

#### Number of variable at split ####
This field is the number of variable that is randomly collected to be sampled at each split time. **Accuracy** results will show the results by this randomly selected predictors. The default value is from **1** to **101**. 

* For the T1D experiment, select **41** and **81** in this field.

### Model Results ###

#### Train Error ####
The error rate of prediction during training a model is given by the plot under error rate.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/trainerror.PNG) 

#### Test Error ####
The error rate of prediction on test set is a better estimate of model accuracy and it can be estimated using confusion matrix generated under confusion Matrix tab. The Statistics of how well the model performs can also be obtained using the statistics tab.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/testerror.PNG)
![](https://github.com/BioHPC/MegaR/blob/master/screenshot/teststats.PNG)

#### Important feature ####
From a practical perspective, it is important to identify features that are important in identifying the class of metagenomic sample. The top ten important species or genus crucial in identifying the class of sample along with their variable importance is shown under the **Important Feature** tab.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/features.PNG)

#### Accuracy ####
For the random forest model, a user can select the number of predictors to be used during each split. Similarly, users can also select the range of cost to be applied to support vector machines. A plot for the accuracy of the model based on selected parameter can be seen in accuracy tab. MegaR selects the best accuracies from among the selected parameter for model building. An additional feature of tool that can improve accuracy is the **Select level to classify** tab. When more than two classes are present, only the classes that are examined for classification can be selected. This also allows the removal of control and other less important classes from the model, thus increasing the model accuracy.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/accuracy.PNG)

#### AUC ####
AUC - ROC curve is a performance measurement for classification problem at various thresholds settings. MegaR visualize the AUC-ROC plot. 

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/AUC.PNG)

#### Download ####
Finally, MegaR provides the option to download the trained model for later use in Prediction. If a user clicks **Download Model**, the RDS model file is generated and downloaded. A user can load this model for the fast prediction of unknown samples without training the model again.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/download.PNG)

### Validation ###
Cross-validation is a manner to access, judge, and review the performance of machine learning models. First and foremost, cross validation is essential to validate the model accuracy and model bias. This implies that the developed model should not be overfitted and not having bias. 
To make a better model, all data set is not usually used for the training purpose, but split into training and validating/testing sets.  For example, in k-fold cross validation, the dataset is shuffled and divided into k sub samples. The k-1 samples are used as a training dataset and the single partition is used for validation. This process is repeated k times to represent the model performance. MegaR provides cross validation options allowing for an accurate prediction measure. The variance in fitting the model tends to be higher if it is fitted to a small dataset, therefore k-fold cross validation can have a high variance. MegaR provides users to select N independent runs of the 10-fold cross validation to minimize such a high variance.

* For the T1D experiment, provide **3** for this field.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/Validation.PNG)
 
### Prediction ###
Finally, we can upload unknown test set and get the prediction on which category they fall into as a list. Unknown set must be biom file if model is developed from qiime data and merged metaphlan table if model is developed from the metaphlan data. 

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/prediction.PNG)

It is also possible to upload a previously trained model for prediction using the **Use custom model** tab under **Prediction**. If a user click the **Prediction** and select **Use cumstome model**, then a user can upload the downloaded RDS model (trained model) along with the unknown dataset to predict the phenotype fast without training the model again.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/Prediction_CustomModel3.png)
