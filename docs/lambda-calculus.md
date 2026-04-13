# Essential Lambda Calculus

## Our grammar

$$
\begin{align*}
e &\mathtt{::=\ } \mathtt{C} & \texttt{[constant]}
\\
&\mathtt{|\ \ \ \ } x & \texttt{[variable]}
\\
&\mathtt{|\ \ \ \ } (e\ e) & \texttt{[application]}
\\
&\mathtt{|\ \ \ \ } (\lambda x.e) & \texttt{[lambda]}
\\
&\mathtt{|\ \ \ \ } (\mathtt{letrec}\ (b_0\dots b_n)\ e) & \texttt{[letrec]}
\\
b &\mathtt{::=\ } \lbrace x\ \lambda y.e \rbrace & \texttt{[letrec binding]}
\end{align*}
$$

## Free variables $\mathcal{F}$

A variable is free in an expression if it is present in a scope where it is not bound.

$$
\begin{align*}
\mathcal{F}\mathtt{C} &= \set{}
\\
\mathcal{F}x &= \set{x}
\\
\mathcal{F}(e_0\ e_1) &= \mathcal{F}e_0\cup \mathcal{F}e_1
\\
\mathcal{F}(\lambda x.e) &= \mathcal{F}(e) - \set{ x }
\\
\mathcal{F}(\mathtt{letrec}\ (\lbrace x_0\ \lambda_0\rbrace\dots\lbrace x_n\ \lambda_n\rbrace)\ e) &=
\Big(\bigcup_{i=0}^{i=n}\mathcal{F}\lambda_i \cup \mathcal{F}(e)\Big) - \set{x_0\dots x_n}
\end{align*}
$$

## Substitution $\mathcal{S}[x/e]$

$$
\begin{align*}
\mathcal{S}[x/e]\mathtt{C} &= \mathtt{C}
\\
\mathcal{S}[x/e]x &= e
\\
\mathcal{S}[x/e](e_0\ e_1) &= (\mathcal{S}[x/e]e_0\ \mathcal{S}[x/e]e_1)
\\
\mathcal{S}[x/e](\lambda x.e_0) &= (\lambda x.e_0)
\\
\mathcal{S}[x/e](\lambda y.e_0) &= (\lambda y.\mathcal{S}[x/e]e_0)
\\
\mathcal{S}[x/e](\mathtt{letrec}\ (b_0\dots b_n)\ e) &=
(\mathtt{letrec}\ (\mathcal{S}[x/e]b_0\dots \mathcal{S}[x/e]b_n)\ \mathcal{S}[x/e]e)
\\
\mathcal{S}[x/e]\lbrace a\ \lambda b.e\rbrace &= \begin{cases} \lbrace a\ \lambda b.\mathcal{S}[x/e]e\rbrace &\text{if } x \not \in \set{a,b}
\\
\lbrace a\ \lambda b.e\rbrace &\text{otherwise}
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
\beta((\lambda x.e_0)\ e_1) &= \mathcal{S}[x/\beta e_1]\beta e_0
\\
\beta (e_0\ e_1) &= (\beta e_0\ \beta e_1)
\\
\beta (\lambda x.e) &= (\lambda x . \beta e)
\\
\beta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\beta b_0 \dots \beta b_n)\ \beta e)
\\
\beta \lbrace x\ \lambda y.e \rbrace &= \lbrace x\  \lambda y . \beta e \rbrace
\end{align*}
$$

## Eta Reduction $\eta$

$$
\begin{align*}
\eta\mathtt{C} &= \mathtt{C}
\\
\eta x &= x
\\
\eta (e_0\ e_1) &= (\eta e_0\  \eta e_1)
\\
\eta(\lambda x.(e\ x)) &= \begin{cases}
\eta e &\text{iff } x \not \in \mathcal{F}e
\\
(\lambda x .\eta(e\ x)) &\text{otherwise}
\end{cases}
\\
\eta(\lambda x.e) &= (\lambda x.\eta e)
\\
\eta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\eta b_0 \dots \eta b_n)\ \eta e)
\\
\eta \lbrace x\ \lambda y.e \rbrace &= \lbrace x\  \lambda y . \eta e \rbrace
\end{align*}
$$

## Tree Shaking $\mathcal{T}$

$$
\begin{align*}
\mathcal{T}\mathtt{C} &= \mathtt{C}
\\
\mathcal{T}x &= x
\\
\mathcal{T}(e_0\ e_1) &= (\mathcal{T}e_0\ \mathcal{T}e_1)
\\
\mathcal{T}(\lambda x . e) &= (\lambda x.\mathcal{T}e)
\\
\mathcal{T}(\mathtt{letrec}\ (\ )\ e) &= \mathcal{T}e
\\
\mathcal{T}(\mathtt{letrec}\ (\lbrace x_0\ \lambda_0\rbrace\dots \lbrace x_n\ \lambda_n\rbrace)\ e) &=
(\mathtt{letrec}\ (\set{\lbrace x_j\ \mathcal{T}\lambda_j\rbrace | x_j \not \in
\bigcup_{i=0}^{i=n}\mathcal{F}\mathcal{T}\lambda_i \cup \mathcal{F}\mathcal{T}e
})\ \mathcal{T}e)
\end{align*}
$$
