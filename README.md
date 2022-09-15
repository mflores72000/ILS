
# <span style="color: green"> About ILS package </span> <img src="man/figures/logo.PNG" align="right" alt="" width=120, height=120 />

<!-- badges: start -->
<a href="https://cloud.r-project.org/web/packages/ILS"
class="pkgdown-release"><img
src="https://www.r-pkg.org/badges/version/pkgdown"
alt="CRAN Status" /></a>
<a href="https://github.com/mflores72000/ILS"
class="pkgdown-devel"><img
src="https://github.com/r-lib/pkgdown/workflows/R-CMD-check/badge.svg"
alt="R-CMD-check" /></a> 
<!-- badges: end -->


<p style="text-align:justify;">The aim of the `ILS` package is to detect laboratories that provide not consistent results, working simultaneously with different test materials, from the perspective of the Univariate Data Analysis and the Functional Data Analysis (**FDA**).</p>

<p style="text-align:justify;">The `ILS` package estimates the Mandel’s $h$ and $k$ univariate statistics, based on the **ASTM E691** and **ISO 5725-2** standards, to identify laboratories that provide significantly different results. $Cochran$ and $Grubbs$ tests to evaluate the presence of outliers are also available. In addition, Analysis of Variance (**ANOVA**) techniques are provided, including the Tukey and F tests to evaluate differences between the means for the corresponding test variable.</p>

<p style="text-align:justify;"> One of the novelties of this package is the incorporation of tools to perform an `ILS` from a functional data analysis approach. Accordingly, the functional nature of the data obtained by experimental techniques corresponding to analytical chemistry, applied physics and engineering applications (spectra, thermograms, and sensor signals, among others) is taking into account by implementing the functional extensions of Mandel’s $h$ and $k$ statistics. For this purpose, the ILS package also estimates the functional statistics $H(t)$ and $K(t)$, as well as the $d_H$ y $d_K$ test statistic, which are used to evaluate the repeatability and reproducibility hypotheses where the critical $c_h$ and $c_k$ values are estimated by using a bootstrap algorithm.</p>

<br>

## <span style="color: green"> References </span>

<br>

<p style="text-align:justify;"> Flores, M., Fernández-Casal, R., Naya, S., Tarrío-Saavedra, J., & Bossano, R. (2018). ILS: An R package for statistical analysis in interlaboratory studies.. *Chemometrics and Intelligent Laboratory Systems* , **181** , 11-20,[DOI](https://doi.org/10.1016/j.chemolab.2018.07.013) </p> 
<br>
