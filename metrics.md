# Disparity metrics

ID | Name | Category | Description | CiteKey | Function | Comment |
---|------|----------|-------------|---------|----------|---------|
1 | Product of variances | variance | average dissimilarity among forms | Wills1994 | `variance.calc` + `prod.apply` | covariance? |
2 | Sum of variances | variance | | Wills1994 | `variance.calc` + `sum.apply` | affected by sample size (Butler2012) |
3 | Product of ranges | range | overall morphological variation | Wills1994 | `range.calc` + `prod.apply` | covariance? |
4 | Sum of ranges | range | | Wills1994 | `range.calc` + `sum.apply` | affected by sample size (Butler2012) |
5 | Mean distance from centroid | centroid |             |      | `centroid.apply` + `cen.apply.mea` |         |
6 | Median distance from centroid | centroid |             |      | `centroid.apply` + `cen.apply.med` |         |

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
With:
* `n` = number of dimensions (i.e. the number of ordination axis). Must be < `k-1`
* `\mathbf{v}_{n}` = the eigen values for the `n` dimensions (i.e. the ordination scores)
* `k` = number of elements (i.e. number of taxa)


#### References
References (i.e. CiteKeys) are available in the [References.bib](https://github.com/TGuillerme/dispRity/blob/master/References.bib) file
