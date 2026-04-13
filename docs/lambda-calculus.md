# Essential Lambda Calculus

## Our grammar

$$
\begin{align*}
& e & \texttt{[expression]}
\\
& \mathtt{C} & \texttt{[constant]}
\\
& x & \texttt{[variable]}
\\
& (e\;e) & \texttt{[application]}
\\
& (\lambda x.e) & \texttt{[lambda]}
\\
& (\mathtt{letrec}\;(b_0\dots b_n)\;e) & \texttt{[letrec]}
\\
& b_i = \llbracket x\;\lambda y.e\rrbracket & \texttt{[letrec binding]}
\end{align*}
$$

## Free variables $\mathcal{F}$

$$
\begin{align*}
\mathcal{F}\mathtt{C} &= \{\ \}
\\
\mathcal{F}x &= \{\ x\ \}
\\
\mathcal{F}(e_0\;e_1) &= \mathcal{F}e_0\cup \mathcal{F}e_1
\\
\mathcal{F}(\lambda x.e) &= \mathcal{F}(e) - \{\ x\ \}
\\
\mathcal{F}(\mathtt{letrec}\;(\llbracket x_0\;\lambda_0\rrbracket\dots\llbracket x_n\;\lambda_n\rrbracket)\;e)
&=
\Big(\bigcup_{i=0}^{i=n}\mathcal{F}\lambda_i
\cup \mathcal{F}(e)\Big)
- \{\ x_0\dots x_n\ \}
\\
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
\mathcal{S}[x/e]\llbracket a\;\lambda b.e\rrbracket &= \begin{cases}
\llbracket a\;\lambda b.\mathcal{S}[z/e]e\rrbracket &\text{if } x \not \in \set{a,b}
\\
\llbracket a\;\lambda b.e\rrbracket &\text{otherwise}
\end{cases}

\end{align*}
$$

## Beta Reduction $\beta$

$$
\begin{align*}
\beta\mathtt{C} &= \mathtt{C}
\\
\beta x &= x
\\
\beta((\lambda x.e_0)\;e_1) &= \mathcal{S}[x/\beta e_1]\beta e_0
\\
\beta (e_0\;e_1) &= (\beta e_0\;\beta e_1)
\\
\beta (\lambda x.e) &= (\lambda x . \beta e)
\\
\beta (\mathtt{letrec}\;(b_0\dots b_n)\; e) &= (\mathtt{letrec}\;(\beta b_0 \dots \beta b_n)\;\beta e)
\\
\beta \llbracket x\;\lambda y.e \rrbracket &= \llbracket x\; \lambda y . \beta e \rrbracket
\end{align*}
$$

## Eta Reduction $\eta$

$$
\begin{align*}
\eta\mathtt{C} &= \mathtt{C}
\\
\eta x &= x
\\
\eta (e_0\;e_1) &= (\eta e_0\; \eta e_1)
\\
\eta(\lambda x.(e\;x)) &= \begin{cases}
\eta e &\text{iff } x \not \in \mathcal{F}e
\\
(\lambda x .\eta(e\;x)) &\text{otherwise}
\end{cases}
\\
\eta(\lambda x.e) &= (\lambda x.\eta e)
\\
\eta (\mathtt{letrec}\;(b_0\dots b_n)\; e) &= (\mathtt{letrec}\;(\eta b_0 \dots \eta b_n)\;\eta e)
\\
\eta \llbracket x\;\lambda y.e \rrbracket &= \llbracket x\; \lambda y . \eta e \rrbracket
\end{align*}
$$

## Tree Shaking $\mathcal{T}$

$$
\begin{align*}
\mathcal{T}\mathtt{C} &= \mathtt{C}
\\
\mathcal{T}x &= x
\\
\mathcal{T}(e_0\;e_1) &= (\mathcal{T}e_0\;\mathcal{T}e_1)
\\
\mathcal{T}(\lambda x . e) &= (\lambda x.\mathcal{T}e)
\\
\mathcal{T}(\mathtt{letrec}\;(\;)\;e) &= \mathcal{T}e
\\[-1em]
\mathcal{T}(\mathtt{letrec}\;(\llbracket x_0\;\lambda_0\rrbracket\dots \llbracket x_n\;\lambda_n\rrbracket)\;e) &=
(\mathtt{letrec}\;(\set{\llbracket x_j\;\mathcal{T}\lambda_j\rrbracket| x_j \not \in
\bigcup_{i=0}^{i=n}\mathcal{F}\mathcal{T}\lambda_i \cup \mathcal{F}\mathcal{T}e
})\;\mathcal{T}e)
\end{align*}
$$
