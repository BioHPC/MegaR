# MegaR: A user-friendly interactive machine learning interface for metagenomic analysis to identify and predict disease sample accurately.
  
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

## Graphical Interface Workflow

In RStudio, use following command:

```
> library(MegaR)
> MegaR() 
```

**Data Input**

MegaR can take both OTU table and BIOM file from popular metagenomic profiling tools, [metaphlan](https://www.nature.com/articles/nmeth.2066) and [qiime](https://www.nature.com/articles/nmeth.f.303).
MegaR provides sample data from the DIABIMMUNE project. If you clone or download the full MegaR package, the data files are located in data folder. Otherwise, the data set used for this example is the T1D dataset that can be downloaded from [here](https://github.com/BioHPC/MegaR/tree/master/data/WGS/T1D.zip). Clicking the browse tab, user can upload the input file from anywhere in the computer path.  After the data is uploaded, the contents of the data are displayed as an interactive table under the **Data** tab. 

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/Datainput.gif)

**Data Preprocessing**

After the data is loaded, it can be preprocessed to allow efficient machine leaning. Click preprocess tab and select the appropriate taxonomic level information to use for machine learning. Genus Level and Species Level tabs return genuses and species level from the dataset as the feature. **All Level** tab tracks back the taxon level for unclassified higher order. The slider bar can be adjusted to select the percentage of sample that should include the threshold amount of abundance present in the data. Finally, there is a choice for normalizing the data. MegaR provides three choixes for data normalization: Quantile, TMM, and none. After choosing the desired normalization, the processed data that is ready for building machine learning models is seen under the data tab.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/preprocessing.gif)

**Model Development**

There are three machine learning models available for classification. Generalized Linear Model (GLM), Random Forest Model and Support Vector Machines. Select the appropriate machine learning model.

Upload the metadata that contains information about the sample. The metadata should be tab separated files with rows containing sample ids and columns containing the other information like class that each sample belongs to. The sample id in the metadata must match exactly to sample id in the initial metaphlan/qiime file. Mention which column in metadata contains the id that matches  the initial metaphlan and qiime result and also the column where the class this sample are stored is mentioned. Then select the percentage of data that you want to use to train a model. One can use as much as 100% of the data but then there will be no test set to generate confusion matrix. The error rate of prediction during training a model is given by the plot under error rate.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/trainerror.PNG) 

The statistics can be viewed using Stats tab.


![](https://github.com/BioHPC/MegaR/blob/master/screenshot/teststats.PNG)

MegaR also provides the user with an AUC graph for further analysis under the **AUC** tab.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/AUC.PNG)

For random forest model, user can select the number of predictors to be used during each split. Similarly, users can also select the range of cost to be applied to support vector machines. A plot for the accuracy of the model based on selected parameter can be seen in accuracy tab. MegaR selects the best accuracies from among the selected parameter for model building.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/accuracy.PNG)

An additional feature of tool that can improve accuracy is the **select level to classify** tab. When more than two classes are present, only the classes that are examined for classification can be selected. This also allows the removal of control and other less important classes from the model, thus increasing the model accuracy.

Plots can be downloaded by clicking the download the plot button to save for future use.

The error rate of prediction on test set is a better estimate of model accuracy and it can be estimated using confusion matrix generated under confusion Matrix tab. The Statistics of how well the model performs can also be obtained using the statistics tab.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/testerror.PNG)

From a practical perspective, it is important to identify features that are important in identifying the class of metagenomic sample. The top ten important species or genus crucial in identifying the class of sample along with their variable importance is shown under the **Important Feature** tab.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/features.PNG)
 
Finally, MegaR provides the option to download the trained model for later use in Prediction. 

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/download.PNG)

**Validation**
 ![](https://github.com/BioHPC/MegaR/blob/master/screenshot/Validation.PNG)
 
**Prediction**

Finally, we can upload unknown test set and get the prediction on which category they fall into as a list. Unknown set must be biom file if model is developed from qiime data and merged metaphlan table if model is developed from the metaphlan data. It is also possible to upload a previously trained model for prediction using the **Use custom model** tab under **Prediction**. Then, a user can upload the downloaded RDS model along with the unknown dataset to predict the phenotype fast without training it again.

![](https://github.com/BioHPC/MegaR/blob/master/screenshot/prediction.PNG)
![](https://github.com/BioHPC/MegaR/blob/master/screenshot/Prediction_CustomModel.PNG)
