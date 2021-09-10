#1. PCA Principal components analysis is a data-reduction technique that transforms a larger number of correlated variables into a much smaller set of uncorrelated variables called principal components
#Whereas EFA exploratory factor analysis is a collection of methods designed to uncover the latent structure in a given set of variables

#PCs are linear combinations of the observed variables. The weights used to form the linear composites are chosen to maximize the variance each principle component accounts for, while keeping the components uncorrelated
#factors of EFA  are assumed to underlie the observed variables, fctors and errors arent directly observable but are inferred from the correlations among the variables
#correlated factors are common but not required in the EFA model

#2. principal components and factor analysis

princomp()
factanal()#these two are functions of base installation of R

#the psych package offer many more useful options than the base counterparts

#functions in psych package

#principal() principal components analysis with optional rotation
#fa() factor analysis by principal axis, minimum residual, weighted least squares or maximum likelihood
#fa.parallek() scree plots with parallel analyses
#fa.plot () plot the results of a factor or principal components analysis
#scree() scree plot for factor and principal components analysis
#fa.diagram() gram factor or principal components loading matrices

#3. common steps of EFA(and to a lesser degree PCA)
#a. prepare the data, input either the raw data matrix or the correlation matrix to the principal() and fa() functions.
#if the data is raw, the correlation matrix is automatically calculate, be sure to screen the data for missing values before proceeding

#b. select a factor model, decide whether PCA(data reduction) or EFA(uncovering latent structure) is a better fit for goals.
#if selecting an EFA  approach we need to choose a specific factoring method(for example, maximum likelihood)

#c. decide how many components/factors to extract

#d, extract the components/factors

#e. rotate the components/factors

#f. interpret the results

#g. compute components or factor scores

#4. principal components
#the goal of PCA is to replace a large number of correlated variables with a smaller number of uncorrelated variables while capturing as much information in the original variables as possible
#these derived variables called principal components are linear combinations of the observed variables
#specifically, the first principal component
PC1=axX12+a2X2+...akXk #is the weighted combination of the k observed variables that accounts fgor the most variance in the original set of variables
#the second component is the linear combination that accounts for the most variaance in the original variables, under the constraint that it is orthogonal(uncorrelated) to the first principal component
#each component maximizes the variance accounted for, while at the same time remaining uyncorrelated with all previous components
#we hope there are a much smaller set of components that can approximate the full set of variables


#5. selecting the number of components to extract

#several criteria are available for deciding how many components to retain in a PCA
#a. basing the number of components on prior experience and theory
#b. selecting the number of components needed to account for some threshold cumulative amount of variance in the variables(for example, 80%)
#c. selecting the number of components to retain by examining the eigenvalues of the kxk correlation matrix among the variables

#the most common approach is based on the eigenvalues, the first PC is associated with the largest eigenvalue, the second PV with the second largest eigenvalue and so on
#the Kaiser-Harris criterion suggests retaining components with eigenvalues greater than , since components with eignevalues less than 1 expalin less variance than contained in a single variable
#the Cattell Scree test, the eigenvalues are plotted against their component numbers. Such plots typically demonstrate a bend or elbow, and the components above this sharp break are retained 
#fanally, we can run simulations, extracting eigenvalues from random data matrices of the same size as original matrix.
#if an eigenvalue based on real data is larger than the average corresponding eigenvalues from a set of random data matrices, that component is retained
#the approach is called parallel analysis(see Hayton, Allen, and Scarpello, 2004, for more details)

install.packages("psych")
library(psych)
fa.parallel(USJudgeRatings[,-1],fa="pc",n.iter=100,
show.legend=F,main="Scree plot with parallel analysis"
)
#the plot displays the scree test based on the observed eigenvalues
#the mean eigenvalues derived from 100 random data matrices and the eigenvalues greater tha n1 criteria(as a horizontal line at y=1)
#all three criteria suggest that a single component is appropriate for summarizing this dataset
#next steo is to extract the principal component using the principal() function

#6. extracting principal components
principal(r, nfactors=, rotate=, scores=) #r is a correlation matrix or a raw data matrix
#nfactors specifies the number of principal components to extract(1 by default)
#rotate indicates the rotation to be applied(varimax by default)
#scores specifies whether to calculate principal-component socres(false by default)

library(psych)
pc<-principal(USJudgeRatings[,-1],nfactors=1)
pc
#inputting the raw data without the CONT variable and specifying that one unrotated component should be extracted 
#since PCA is performed on a correlation matrix, the raw data is automatically converted to a correlation matrix before the components are extracted
#the colum labeled PC1 contains the component loadings, which are the coprrelations of the observed variables with the principal components
#if extracting more than one principal component, there would be columns for PC2, PC3 and so on.

#component loadings are used to interpret the meaning of components
#we can see each variable are highly correlated with the PC1

#the column labeled h2 contains the component communalities, the amount of variance in each variable explained by the components
#u2 contains the component uniquenesses, the amount of variance in physical ability(PHYS)ratings is accounted for by the first PC, and 20% isn't

#the row labeled SS loadings contains the eigenvaues associated with the components.
#eigenvalues are the standardized variance associated with a particular component(in this case, the value for the first component is 10)

#finally, the row labeled Proportion Var represents the amount of variance accounted for by each components


#7. Case study: dataset is Harman23.cor which contans data on 8 body measurements for 305 girls, we wish to replace the original variables with smaller number of derived vairabels

library(psych)
fa.parallel(Harman23.cor$cov, n.obs=302, fa="pc",n.iter=100,
            show.legend=F, main="Scree plot with parallel analysis")
#two  component solution is suggested

#extracts the first two principal components from the correlation matrix
library(psych)
pc<-principal(Harman23.cor$cov, nfactors=2, rotate="none")
pc
#the PC1 accountes for 58% of the variance in the physical measurements, whereas the second component accounts for 22%, totally 81% of the variance

#the frist component correlateds positively with each one to be a general size factor, the second contrastst he first four variables with the second four variables.
#it therefore appears to be a length-versus-volume factor
#whenever two or more components have been extracted, we can rotate the solution to make it more interpretable

#8.rotating principal components
#rotations are a set of mathematical techniques for transforming the component loading matrix into one that is more interpretable
#by purifying the components as much as possible
#it differs with regard to whether the resulting components remain uncorrelated(orthogonal rotation) or are allowed to correlated(oblique rotation)
#also differs in their definition of purifying

#the most popular orthogonal rotation is the varimax rotation, which attempts to purify the columns of the loading matrix, so that each component is defined by a limited set of variables(that is , each colum has a  few large loadings and many very small loadings)

rc<-principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
rc

#the names change from PC to RC to denote rotated components
#the first component is primarily defined by the first four variables(length variables)
#the RC2 indicates the second component is primarily defined by variables 5 through 8(valume variables

#the rotated solution expalines the variables equally well because the variable communalities havent changed

#the cumulative variance accounted for by the two-component rotated solution 81% hasnt changed
#but the proportion of variance accounted for by each individual component has changed(from 58% to 44% for component 1 and from 22% to 37% for component 2)

#this spreading out of the variance across components is common, and technically we should now call them components rather than principal components because the variance-maximizing properties of individual components haven't been retained 

#9. obtaining principal components scores

principal()#obtain scores for each participant on derived variable

library(psych)
pc <-principal(USJudgeRatings[,-1], nfactors=1, score=T)
head(pc$scores)
##the principal component scores are saved in the scores element of the object returned by the principal() function when the option scores=T
#if we wanted, we could get the correlation between the number of contacts occurring between a lwayer and a judge and their evaluation of the judge using
cor(USJudgeRatings$CONT,pc$score
    )
#apparently, there is no relationship between the lawyer's familiarity and their opinions

library(psych)
rc<-principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
round(unclass(rc$weights),2)
#when the principal components analysis is based on a correlation matrix and the raw data arent available, getting principal component scores for each observation is clearly not possible
#but we can get the coefficients used to calculate the principal components

#RC1=0.28height+0.30arm.span+0.30forearm+0.28lower.leg-0.06weight-0.08bitro.diameter-0.10chest.girth-0.04chest.width
#RC2=-0.05height-0.08arm.span-0.09forearm-0.06lower.leg+0.33weight+0.32bitro.diameter+0.34chest.girth+0.27chest.width

#w can take the first composite variable as the mean of the standardized scores for the first four variables
#define the second composite variable as the mean of the standardized scores for teh second four variables

#10. Little Jiffy conquers the world
#there is quite a bit of confusion among data analysts regarding PCA adn EFA
#one reason is for this is historical and can be traced back to a program called little jiffy
#learn more about the PCA/EFA confusion, see Hayton, Allen and Scarpello, 2004


#11. exploratory factor analysis
#EFA goal is to expalin the correlations among a set of observed variables by uncoverring a smaller set of more fundamental unobserved variables underlying the data
#these hypothetical, unobserved variables are called factors.
#each factor is assumed to expalin the variance shared among two or more observed variables, so technically, they are called common factors

#the model can be represented as 
Xi=a1F1+a2F2+...+apFp+Ui
#where Xi is the ith observed v ariable, Fj are the common factors. Ui is the portion of variable Xi unique to that variable(not explained by the common factors)
#ai can be seen as the degree to which each factor contributers to the composition of an observed variable

options(digits=2)
covariances <-ability.cov$cov
correlations <-cov2cor(covariances)
correlations

#nonverbal measure of general intelligence(general)
#a picture-completion  test(picture)
#a block design test(blocks)
#a maze test(maze)
#a reading comprehension test(reading)
#a vocabulary test(vocab

#12. deciding how many common factors to extract
library(psych)
covariances <-ability.cov$cov
correlations<-cov2cor(covariances)
fa.parallel(correlations,n.obs=112, fa="both", n.iter=100,
             main="Scree plots with parallel analysis")
#fa="both" means that we apply for both a principal-components and common-factor approach

#if we would take a PCA approach, we might have choson one component(scree test, parallel analysis) or two components(eigenvalues greater than 1)
#when in doubt, it is usually a better idea to overfactor than to underfactor

#Looking at the EFA  results, a two-factor solution is clearly indicated
#For EFA, the Kaiser-Harris criterion is number of eigenvalues above 0, rather than 1.

#13. extracting common factors

fa(r, nfactorsn.obs=, rotate=, scores=,fm=)
#where r is a correlation matrix or a raw data matrix
#nfactors specifies the number of factors to extract(1 by default)
#n.obs is the numberf of observations(if a correlation matrix is input)
#rotate indicates the rotation to be applied(oblimin by default)
#scores specifies whether or not to calculate factor scores(false by default)
#fm specifies the factoring method(minres by default)

#unlike PCA,there are many methods of extracting common factors, they include maximum likelihood(ml), iterated principal axis(pa), weighted least square(wls), generalized weighted least squares(gls), and minimum residual(minres)

#statisticians prefer the maximum likelihood approach because of its well defined statistical model
#sometimes, maximum likelihood approach fails to converge, in which vase the iterated principal axis option often works well
#to learn more about the different approaches, see Mulaik(2009) and Gorsuch(1983)

install.packages("GPArotation")
library(GPArotation)
fa<-fa(correlations, nfactors=2, rotate="none", fm="pa")
fa
#we can see the two factors account for 60% of the variance in the six psychological tests
#when we examine the loadings though, they arent easy to interpret.
#rotation them should help

#14 rotating factors
#we can rotate the two factor solution using either an orthogonal rotation or an oblique rotation

#first try an orthogonal rotation
fa.varimax<-fa(correlations, nfactors=2, rotate="varimax",fm="pa")
fa.varimax

#factors are certainly easier to interpret
#reading and vocabulary load on the first factor and picture compeltion, block design and mazes load on the second factor
#the general noverbal intelligence measures loads on both factors
#this may indicate a verbal intelligence factor and a nonverbal intelligence factor


#try an oblique(correlated) rotation such as promax
fa.promax <-fa(correlations, nfactors=2, rotate="promax", fm="pa")
fa.promax

#several difference exist between the orthogonal and oblique solutions
#in an orthogonal solution, attention focuses on the factor structure matrix(the correlations of the variables with the factors)
#in an obilque solution, there are three matrices to consider: the factor structure matrix, the factor pattern matrix, and the factor intercorrelation matrix

#the factor patter nmatrix is a matrix of standardized regression coefficients, they give the weights for predicting the variables from the factors
#the factor intercorrelation matrix gives the correlations among the factors

#if the factor intercorrelations had been low, we might have gone back to an orthogonal solution to keep things simple
#the factor structure matrix(or factor loading matrix) isnt provided but we can easily calculate it using the formula F=P*Phi, where F is the factor loading matrix, P is the factor pattern matrix, and Phi is the factor 

#this function is carrying out the multiplication of factor pattern matrix and factor intercorrelation matrix
fsm<-function(oblique){
  if(class(oblique)[2]=="fa"&is.null(oblique$Phi)){
    warning("Object doesnt look like oblique EFA")
  }
  else{
    P<-unclass(oblique$loading)
    F<- P %*% oblique$Phi
    colnames(F)<-c("PA1","PA2")
    return(F)
  }
}

fsm(fa.promax)
fa.promax

#graph an orthogonal or oblique solution using the factor.plot() or fa.diagram()
factor.plot(fa.promax, labels=rownames(fa.promax$loadings))

fa.diagram(fa.promax, simple=F)
#if simple=T, only the largest loading per ietm is displayed
#it shows the largest loadings for each factor, as well as the correlations between the factors
#this type of diagram is helpful when there are several factors

#15. factor scores

#compared with PCA, the goal of EFA is much less likely to be the calculation of factor scores, but these scores are easily obtained from the fa() function by including the score=T option(when the raw data are available)
#scoring coefficients(standardized regression weights) are available in the weights element of the oebject returned

#ability.cov dataset, we can obtain the beta weights for calculating the factor scores estimates for the two factor oblique solution using
fa.promax$weights
#unlike components scores, whic hare calculated exactly, factors scores can only be estimated 
#several methods exist
#fa() function uses the regression approach.
#to learn more about factor scores, see DiStefano, Zhu, and Mindrila(2009)


#16. other EFA-related package
#FactoMineR package provides methods for PCA and EFA as well as other latent variable models, that is also considering what we didnt consider like both numeric and categorical variables
#FAiR package estimates factor analysis models using a genetic algorithm that permits the ability to impose inequality restrictions on model paramters
#GPArotation package offers many additional factor rotation methods
#nFactors package offer sophisticated techniques for determining the number of factors underlying data

#17. other latent variabel models

#test the theory against a set of collected data, like how many factors underlie a set of variables, how the variables load on those factors and how the factors correlated with one another.
#use the approach called confirmatory factor analysis(CFA)

#CFA is a subset of a methodology called structural equation modeling(SEM), SEM allows to posit not only the number and composition of underlying factors but also how these factors impact one a nother
#we can think of SEM as a combination of confirmatory factor analses(for the variables)and regression analyses(for the factors)
#the resulting output includes statistical tests and fit indices
#packages include sem, OpenMx and lavaan for CFA and SEM performing

#ltm package can be used to fit latent models to the items contained in the test and questionnaires
#often used to create large-scale standardized tests
#examples include the Scholastic Aptitude Test(SAT) and the Graduate Record Exam(GRE)

#Latent class models where the underlying factors are assumed to be categorical rather than continuous can be fit with FlexMix, lcmm, randomLCA and poLCA package
#lcda package performs latent class discriminant analysis and the lsa package performs latent semantic analysis, a methodology used in natural language processing

#ca package provides functions for simple  and multiple correspondence anlaysis, these methods allow you to explore the structure of categorical variables in two way and multiway tables

#multidimensional scaling(MDS) is to detect underlying dimensions tha texplain the similarities and distances between a set of mweasured objects. The cmdscale() function in the base installation performs a cLasscial MDS
#WHEREAS THE isoMDS() function in the MASS package performs a nonmetric MDS
#the vegan package also contains functions for classical and nonmetric MDS

#18. summary of methods(very important)
#check the graph in P367/P628
