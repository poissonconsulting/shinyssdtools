このアプリは、濃度データに種の感受性分布を適合させます。
このアプリは、Rパッケージ[ssdtools](https://poissonconsulting.github.io/ssdtools/)から構築されており、同じ機能を共有しています。
ssdtoolsへの関連する変更に従って更新および再デプロイされます。
このアプリを使用して生成されたHC5推定値を報告する場合、ssdtoolsのバージョンとデータセットに適合した分布の名前をリストすることをお勧めします。

適合度表の列は、分布(dist)、Anderson-Darling統計(ad)、Kolmogorov-Smirnov統計(ks)、Cramer-von Mises統計(cvm)、赤池情報量基準(aic)、サンプルサイズで補正された赤池情報量基準(aicc)、ベイズ情報量基準(bic)、AICc差(delta)、およびAICcベースの赤池ウェイト(weight)です。
予測は、適合のモデル平均(aiccを使用)推定値です。
パーセントハザード濃度は、テストされた種のそのパーセンテージに影響を与えると予測される化学物質の濃度です。

バグ、予期しない動作を報告したり、機能をリクエストしたりするには、[こちらのGitHub issueを提出してください](https://github.com/poissonconsulting/shinyssdtools/issues)。

ssdtoolsパッケージを出版物で引用するには、次を使用してください:
Thorley, J., Fisher, R., Fox, D., and Schwarz, C. 2025. ssdtools v2: An R package to fit Species Sensitivity Distributions. JOSS 10(105): 7492. doi:10.21105/joss.07492.

ウェブアプリを引用するには、次を使用してください:
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/

モデル平均を使用してHC5推定値を生成する方法の詳細については、以下を参照してください:
[Schwarz, C.J. and A.R. Tillmanns. 2019. Improving statistical methods to derive species sensitivity distributions. Water Science Series, WSS2019-07, Province of British Columbia, Victoria.](http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf)
