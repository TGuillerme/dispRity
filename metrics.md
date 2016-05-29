# Disparity metrics

ID | Name | Category | Description | CiteKey | Function | Comment |
---|------|----------|-------------|---------|----------|---------|
1 | Product of variances | variance | average dissimilarity among forms | Wills1994 | `variance.calc` + `prod.apply` | covariance? |
2 | Sum of variances | variance | | Wills1994 | `variance.calc` + `sum.apply` | affected by sample size (Butler2012) |
3 | Product of ranges | range | overall morphological variation | Wills1994 | `range.calc` + `prod.apply` | covariance? |
4 | Sum of ranges | range | | Wills1994 | `range.calc` + `sum.apply` | affected by sample size (Butler2012) |
5 | Mean distance from centroid | centroid (distance?) |             | Wills1994 | `centroid.apply` + `cen.apply.mea` | ? |
6 | Median distance from centroid | centroid (distance?) |             |      | `centroid.apply` + `cen.apply.med` | ? |
7 | PCO Volume | volume | corrected product of the two largest eigenvalues | Ciampaglio2001 |   | why only the two first eigenvalues? |
8 | Ellipsoid Volume | volume | | Wills1994, Donohue2013 | | ecology |
9 | Convex Hull volume | volume | | | | |
10 | Mean/median inter-taxa distance | distance | | | | |

Note that some metrics can be combined (in a more or less silly way). For example, the Ellipsoid Volume/Convex Hull can be a good way to measure the relative volume occupancy (i.e. the "average" volume / by the total volume); or the sum of distance from centroid (illustrating the spread of the data from the mean?).


Metrics from Anderson et al 2012 (Nature):
 * (1) sum of univariate ranges;
 * (2) root product of univariate ranges;
 * (3) range as maximum euclidean distance;
 * (4) area of the convex hull (coordinate axes 1 and 2 only);
 * (5) volume;
 * (6) sum of variances;
 * (7) root product of variances;
 * (8) mean pairwise distance;
 * (9) median pairwise distance;
 * (10) mean distance to the centroid (Navarro, 2001)

<!--
   |      |          |             |         |          |         |
ID: arbitrary number of the metric
Name: name of the metric
Category: which type of metric
Description: a brief description of what does it measure
CiteKey: the citeKey from the BibTeX file
Function: the functions as in dispRity_fun
Comment: biases, benefits, etc...
-->

#### Formulas
This is a list of the formula of each functions in LaTeX. See [here for LaTeX formula](https://en.wikibooks.org/wiki/LaTeX/Mathematics) and [here for LaTeX math symbols](http://web.ift.uib.no/Teori/KURS/WRK/TeX/symALL.html).
<!--
Don't forget to add the arguments of the formula at the bottom.
-->
##### 1 - Product of variances
```
\prod{\sigma^{2}{n}}
```
##### 2 - Sum of variances
```
\sum{\sigma^{2}{n}}
```
##### 3 - Product of ranges
```
\prod{(max{n}-min{n})}
```
##### 4 - sum of variances
```
\sum{(max{n}-min{n})}
```
##### 5 - Mean distance from centroid 
```
\frac{\displaystyle\sqrt{\sum_{i=1}^{k}{(\mathbf{v}_{n}-Centroid_{n})^2}}}{k} 
```
##### 6 - Median distance from centroid 
```
Md{\displaystyle\sqrt{\sum_{i=1}^{k}{(\mathbf{v}_{n}-Centroid_{n})^2}}}
```
##### 7 - PCO Volume 
```
\frac{\lambda_{1}\lambda_{2}}/{k^{2}}
```
##### 8 - Median distance from centroid 
```
\frac{\pi^{n/2}}/{\Gamma(\frac{n}{2}+1)}\prod{\lambda_{i}^{0.5}}
```
With:
* `n` = number of dimensions (i.e. the number of ordination axis). Must be < `k-1`
* `\mathbf{v}_{n}` = the eigen values for the `n` dimensions (i.e. the ordination scores)
* `k` = number of elements (i.e. number of taxa)
* `\lambda_{n}` = the eigenvalue of the n<sup>th</sup> dimension. Note that weirdly the eigenvalue is equal to the sum of the variance/covariance within each axis multiplied by the maximum number of dimensions (max = k-1). But maybe this is only in ordinations from (euclidean based?) distance matrices?
* `\Gamma` = is the gamma distribution

#### References
References (i.e. CiteKeys) are available in the [References.bib](https://github.com/TGuillerme/dispRity/blob/master/References.bib) file.
