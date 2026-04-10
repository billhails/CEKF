# Essential Lambda Calculus

## Our grammar

$$
\begin{align*}
e &= \mathtt{C} &\texttt{[constant]}
\\
e &=  x &\texttt{[variable]}
\\
e &=  (e\;e) &\texttt{[application]}
\\
e  &= (\lambda x.e) &\texttt{[lambda]}
\\
e &= (\mathtt{letrec}\;(b_0\dots b_n)\;e) &\texttt{[letrec]}
\\
b &= (x\;\lambda y.e) &\texttt{[letrec binding]}
\end{align*}
$$

## Free variables $\mathfrak{FV}$

$$
\begin{align*}
\mathfrak{FV}\mathtt{C} &= \set{}
\\
\mathfrak{FV}x &= \set{x}
\\
\mathfrak{FV}(e_0\;e_1) &= \mathfrak{FV}e_0\cup \mathfrak{FV}e_1
\\
\mathfrak{FV}(\lambda x.e) &= \mathfrak{FV}(e) - \set{x}
\\
\mathfrak{FV}(\mathtt{letrec}\;((x_0\;\lambda_0)\dots(x_n\;\lambda_n))\;e)
&= \bigg(\Big(\bigcup_{i=0}^{i=n}\mathfrak{FV}\lambda_i\Big) -
\set{x_0\dots x_n}\bigg) \cup \mathfrak{FV}(e)
\end{align*}
$$

## Substitution $\mathcal{S}[x/e]$

$$
\begin{align*}
\mathcal{S}[x/e]\mathtt{C} &= \mathtt{C}
\\
\mathcal{S}[x/e]x &= e
\\
\mathcal{S}[x/e](e_0\;e_1) &= (\mathcal{S}[x/e]e_0\;\mathcal{S}[x/e]e_1)
\\
\mathcal{S}[x/e](\lambda x.e_0) &= (\lambda x.e_0)
\\
\mathcal{S}[x/e](\lambda y.e_0) &= (\lambda y.\mathcal{S}[x/e]e_0)
\\
\mathcal{S}[x/e](\mathtt{letrec}\;(b_0\dots b_n)\;e) &=
(\mathtt{letrec}\;(\mathcal{S}[x/e]b_0\dots \mathcal{S}[x/e]b_n)\;\mathcal{S}[x/e]e)
\\
\mathcal{S}[x/e](x\;\lambda y.e) &= (x\;\lambda y.e)
\\
\mathcal{S}[z/e](x\;\lambda y.e) &= (x\;\lambda y.\mathcal{S}[z/e]e)
\end{align*}
$$

## Beta Reduction $\beta$

$$
\beta((\lambda x.e_0)\;e_1) = \mathcal{S}[x/e_1]e_0
$$

## Eta Reduction $\eta$

$$
\eta(\lambda x.(e\;x)) = e\text{ iff }x \not \in \mathfrak{FV}e
$$

## Tree Shaking $\mathcal{T}$

$$
\begin{align*}
\mathcal{T}(\mathtt{letrec}\;(\;)\;e) &= e
\\
\mathcal{T}(\mathtt{letrec}\;(\set{b_0\dots b_n})\;e) &=
(\mathtt{letrec}\;(\set{(b_0\;\lambda_0)\dots(b_n\;\lambda_n)} \cap B)\;e)
\\
& \text{where } B = \mathfrak{FV}(\lambda_0\dots\lambda_n) \cup \mathfrak{FV}e
\end{align*}
$$
