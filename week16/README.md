# Week 16: Lasso Regression with CV, Extra: Intro to Unsupervised learning (PCA)

We'll keep thinking about Lasso/Ridge regression for Linear Regression, think a little bit about unsupervised learning, and talk about "where to go from here".


## Class outline

 1. [Lecture](lecture16_s2020_toupload.pdf)
 1. [Notebook: Model selection with Lasso Regression and CV](prep_model_selection_week16.ipynb)
	* Data:  ozone2.csv([view](ozone2.csv), [raw](https://raw.githubusercontent.com/jnaiman/is542_spring2020/master/week07/ozone2.csv)) (Ozone data from MIS)
    * See [as an Rscript](Rscripts/prep_model_selection_week16.R) 

	

## Reading

Lasso Regression + CV: ISL 6.0-6.5; Intro to Unsupervised learning (PCA): ISL 10.0-10.2

**Optional** reading: https://www.datacamp.com/community/tutorials/pca-analysis-r

**Optional** reading (Lasso/Ridge regression): https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net, https://www.rstatisticsblog.com/data-science-in-action/lasso-regression/

**Optional** reading (Model selection): http://r-statistics.co/Model-Selection-in-R.html

Packages to install (with command: install.packages("PACKAGE\_NAME") or install.packages(PACKAGE\_NAME) ):

 1. stats
 1. devtools
 1. ggbiplot - after installing devtools you can try installing this with: install_github("vqv/ggbiplot")
 1. tidyverse
 1. glmnet
 1. plotmo
 1. psych
 
## Extra Resources

 1. Extra R-script about forward and backward selection, [optional_backward_forward_selection_week16.R](Rscripts/optional_backward_forward_selection_week16.R)
 1. Extra R-script with PCA, [prep_pca_week16.R](Rscripts/prep_pca_week16.R)

 
# Where to go from here

Possible iSchool classes of interest (note: not all 490/590 classes offered all semesters/years):
 * IS457 - supervised & unsupervised learning  
 * IS490RB - data science, IS490RB2 - advanced data science
 * IS590DT - data mining
 * IS590MD - a higher level ML course, IS590ML is a Machine Learning (ML) team projects course, IS590MSC is advanced topics in ML & social computing

Textbooks/Tutorials/Courses:
 * Hands-on Machine Learning (Python, this was referenced in class) - https://www.amazon.com/Hands-Machine-Learning-Scikit-Learn-TensorFlow
 * Deep learning tutorials and examples (and image processing as well, Python) - https://www.pyimagesearch.com/
 * Google's Machine Learning Crash Course (~20-30 hrs, free) - https://developers.google.com/machine-learning/crash-course
 * AWS ML training (some free, some not, Python & R): https://aws.amazon.com/training/learning-paths/machine-learning/, you can find a lot of tutorials by searching for "https://rstudio-pubs-static.s3.amazonaws.com/"

Popular Literature:
 * Dataclysm: https://www.amazon.com/Dataclysm-Identity-What-Online-Offline-Selves
 * Weapons of Math Destruction: https://www.amazon.com/Weapons-Math-Destruction-Increases-Inequality
 * Algorithms of Oppression: https://www.amazon.com/Algorithms-Oppression-Search-Engines-Reinforce
 * Everybody Lies: https://www.amazon.com/Everybody-Lies-Internet-About-Really

## References
 
 * https://idc9.github.io/stor390/notes/cross_validation/cross_validation.html
 * http://ricardoscr.github.io/how-to-use-ridge-and-lasso-in-r.html
 * https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
