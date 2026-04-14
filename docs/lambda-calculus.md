# Essential Lambda Calculus

## Our grammar

$$
\begin{align*}
e\ &\mathtt{::=\ } \mathtt{C} & \texttt{[constant]}
\\
&\mathtt{|\ \ \ \ } x & \texttt{[variable]}
\\
&\mathtt{|\ \ \ \ } (\mathtt{if}\ e\ e\ e) & \texttt{[conditional]}
\\
&\mathtt{|\ \ \ \ } (\oplus\ e\ e) & \texttt{[primitive application]}
\\
&\mathtt{|\ \ \ \ } (e\ e) & \texttt{[application]}
\\
&\mathtt{|\ \ \ \ } (\lambda x.e) & \texttt{[lambda]}
\\
&\mathtt{|\ \ \ \ } (\mathtt{letrec}\ (b_0\dots b_n)\ e) & \texttt{[letrec]}
\\
b\ &\mathtt{::=\ } ( x\ \lambda y.e ) & \texttt{[letrec binding]}
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
\mathcal{F}(\mathtt{if}\ e_0\ e_1\ e_2) &= \mathcal{F}e_0\cup \mathcal{F}e_1\cup \mathcal{F}e_2
\\
\mathcal{F}(\oplus\ e_0\ e_1) &= \mathcal{F}e_0\cup \mathcal{F}e_1
\\
\mathcal{F}(e_0\ e_1) &= \mathcal{F}e_0\cup \mathcal{F}e_1
\\
\mathcal{F}(\lambda x.e) &= \mathcal{F}(e) - \set{ x }
\\
\mathcal{F}(\mathtt{letrec}\ (( x_0\ \lambda_0)\dots( x_n\ \lambda_n))\ e) &=
\Big(\bigcup_{i=0}^{i=n}\mathcal{F}\lambda_i \cup \mathcal{F}(e)\Big) - \set{x_0\dots x_n}
\end{align*}
$$

## Substitution $\mathcal{S}[x/e]$

Substitution only replaces free variables.

$$
\begin{align*}
\mathcal{S}[x/e]\mathtt{C} &= \mathtt{C}
\\
\mathcal{S}[x/e]x &= e
\\
\mathcal{S}[x/e](\mathtt{if}\ e_0\ e_1\ e_2) &= (\mathtt{if}\ \mathcal{S}[x/e]e_0\ \mathcal{S}[x/e]e_1\ \mathcal{S}[x/e]e_2)
\\
\mathcal{S}[x/e](\oplus\ e_0\ e_1) &= (\oplus\ \mathcal{S}[x/e]e_0\ \mathcal{S}[x/e]e_1)
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
\mathcal{S}[x/e]( y\ \lambda z.e) &= \begin{cases} ( y\ \lambda z.\mathcal{S}[x/e]e) &\text{if } x \not \in \set{y,z}
\\
( y\ \lambda z.e) &\text{otherwise}
\end{cases}
\end{align*}
$$

## Beta Reduction $\beta$

Replace formal arguments with actual arguments in the body of an anonymous lambda application.

$$
\begin{align*}
\beta\mathtt{C} &= \mathtt{C}
\\
\beta x &= x
\\
\beta (\mathtt{if}\ e_0\ e_1\ e_2) &= (\mathtt{if}\ \beta e_0\ \beta e_1\ \beta e_2)
\\
\beta (\oplus\ e_0\ e_1) &= (\oplus\ \beta e_0\ \beta e_1)
\\
\beta((\lambda x.e_0)\ e_1) &= \mathcal{S}[x/\beta e_1]\beta e_0
\\
\beta (e_0\ e_1) &= (\beta e_0\ \beta e_1)
\\
\beta (\lambda x.e) &= (\lambda x . \beta e)
\\
\beta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\beta b_0 \dots \beta b_n)\ \beta e)
\\
\beta ( x\ \lambda y.e ) &= ( x\  \lambda y . \beta e )
\end{align*}
$$

## Eta Reduction $\eta$

`fn a(x) { b(x) } == b`, basically.
$$
\begin{align*}
\eta\mathtt{C} &= \mathtt{C}
\\
\eta x &= x
\\
\eta (\mathtt{if}\ e_0\ e_1\ e_2) &= (\mathtt{if}\ \eta e_0\  \eta e_1\ e_2)
\\
\eta (\oplus\ e_0\ e_1) &= (\oplus\ \eta e_0\  \eta e_1)
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
\eta ( x\ \lambda y.e ) &= ( x\  \lambda y . \eta e )
\end{align*}
$$

## Tree Shaking $\mathcal{T}$

Also known as Dead Binding Elimination, removes unused functions from a `letrec`.

$$
\begin{align*}
\mathcal{T}\mathtt{C} &= \mathtt{C}
\\
\mathcal{T}x &= x
\\
\mathcal{T}(\mathtt{if}\ e_0\ e_1\ e_2) &= (\mathtt{if}\ \mathcal{T}e_0\ \mathcal{T}e_1\ \mathcal{T}e_2)
\\
\mathcal{T}(\oplus\ e_0\ e_1) &= (\oplus\ \mathcal{T}e_0\ \mathcal{T}e_1)
\\
\mathcal{T}(e_0\ e_1) &= (\mathcal{T}e_0\ \mathcal{T}e_1)
\\
\mathcal{T}(\lambda x . e) &= (\lambda x.\mathcal{T}e)
\\
\mathcal{T}(\mathtt{letrec}\ (( x_0\ \lambda_0)\dots ( x_n\ \lambda_n))\ e) &=
(\mathtt{letrec}\ (\set{(x_j\ \mathcal{T}\lambda_j) | x_j \not \in
\bigcup_{i=0}^{i=n}\mathcal{F}\mathcal{T}\lambda_i \cup \mathcal{F}\mathcal{T}e
})\ \mathcal{T}e)
\\
\mathcal{T}(\mathtt{letrec}\ (\ )\ e) &= \mathcal{T}e
\end{align*}
$$

## Safety

A function is safe to inline if:

1. it is below a (tunable) size limit.
2. it is not recursive.
3. it only occurs once, and that occurrence is at a call site.

### Size $\mathcal{Z}$

Actually this is pretty arbitrary.

$$
\begin{align*}
\mathcal{Z}\mathtt{C} &= 1
\\
\mathcal{Z}x &= 1
\\
\mathcal{Z}(\mathtt{if}\ e_0\ e_1\ e_2) &= 1 + \mathcal{Z}e_0 + \mathcal{Z}e_1 + \mathcal{Z}e_2
\\
\mathcal{Z}(\oplus\ e_0\ e_1) &= 1 + \mathcal{Z}e_0 + \mathcal{Z}e_1
\\
\mathcal{Z}(e_0\ e_1) &= 1 + \mathcal{Z}e_0 + \mathcal{Z}e_1
\\
\mathcal{Z}(\lambda x.e) &= 1 + \mathcal{Z}e
\\
\mathcal{Z}(\mathtt{letrec}\ (b_0\dots b_n)\ e) &= 1 + \Sigma_{i=0}^{i=n}\mathcal{Z}b_i + \mathcal{Z}e
\\
\mathcal{Z}(x\ \lambda y.e) &= 1+ \mathcal{Z}e
\end{align*}
$$

### Non-Recursive

* Only `letrec` can bind recursive functions.

Given
$$
\begin{align*}
l &= (\mathtt{letrec}\ (b_0\dots b_n)\ e)
\\
B &= \set{b_0\dots b_n} = \set{(x_0\ \lambda y_0.e_0)\dots(x_n\ \lambda y_n.e_n)}
\\
K &= \set{x_0\dots x_n}
\\
M &= \set{x_i \mapsto x_j | x_j \in \mathcal{FV}(\lambda y_i.e_i) \cap K}\ \forall b_i\in B
\\
\end{align*}
$$

Then $x$ is recursive if $(x \mapsto x) \in M^+$.

### Count $\mathcal{C}_x$

Count all free occurences of $x$, or only occurences at call sites.

$$
\begin{align*}
\mathcal{C}_x\mathtt{C} &= 0
\\
\mathcal{C}_xy &=\begin{cases}
1 &\text{if } x = y \text{ and we're counting all occurences}
\\
0 &\text{otherwise}
\end{cases}
\\
\mathcal{C}_x(\mathtt{if}\ e_0\ e_1\ e_2) &= \mathcal{C}_xe_0 +\mathcal{C}_xe_1 +  \mathcal{C}_xe_2
\\
\mathcal{C}_x(\oplus\ e_0\ e_1) &= \mathcal{C}_xe_0 + \mathcal{C}_xe_1
\\
\mathcal{C}_x(e_0\ e_1) &=\begin{cases}
 1 + \mathcal{C}_x e_1 &\text{if }x=e_0
\\
\mathcal{C}_xe_0 + \mathcal{C}_xe_1 &\text{otherwise}
\\
\end{cases}
\\
\mathcal{C}_x(\lambda y.e) &= \begin{cases}
0 & \text{if } x = y\text{ (shadowing)}
\\
\mathcal{C}_x e &\text{otherwise}
\end{cases}
\\
\mathcal{C}_x(\mathtt{letrec}\ (b_0\dots b_n)\ e) &= \mathcal{C}_x e + \Sigma_{i=0}^{i=n}\mathcal{C}_x b_i
\\
\mathcal{C}_x(y\ \lambda z.e) &= \begin{cases}
0 & \text{if } x = y \text{ (shadowing)}
\\
\mathcal{C}_x\lambda z.e &\text{otherwise}
\end{cases}
\end{align*}
$$
