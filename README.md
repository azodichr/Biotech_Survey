



## Machine Learning - Multiclass

### Single Feature Logistic Regression models:
```
for i in feat_*; do echo export PATH=/mnt/home/azodichr/miniconda3/bin:\$PATH\;python /mnt/home/azodichr/GitHub/ML-Pipeline/ML_classification.py -df /mnt/home/azodichr/04_Biotech/00_Data/170503_data_BT_clusters.txt -y_name Biotech_cluster_NF_k5 -p 5 -feat $i -alg LogReg -n 10 -cm F -gs T -gs_n 10 -save SF_"$i" >> run_SingFeat.sb; done
```



### All feature models - test algorithms

Note: Test using 5- and 10-fold cross validation - select based on scoring metrics AND sd across cv replicates

```
cd /mnt/home/azodichr/04_Biotech/01_Pred/06_AllFeatures 
declare -a algs=("LogReg" "RF" "SVM" "SVMpoly" "GB")
declare -a cvs=("5" "10")

for alg in "${algs[@]}"; do for cv in "${cvs[@]}"; do echo export PATH=/mnt/home/azodichr/miniconda3/bin:\$PATH\;python /mnt/home/azodichr/GitHub/ML-Pipeline/ML_classification.py -df /mnt/home/azodichr/04_Biotech/00_Data/170503_data_BT_clusters.txt -y_name Biotech_cluster_NF_k5 -p 10 -feat feat_ALL -alg $alg -n 10 -cm T -gs T -gs_n 10 -cv "$cv" -save all_feat_"$alg"_cv"$cv" >> run_all.sb; done; done```
