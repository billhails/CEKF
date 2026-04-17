# Essential Lambda Calculus

Just a little digression, I think it's interesting
how much of the transformational logic in the pipeline
can be reduced to pure math on a lambda calculus.

## Our grammar

This is basically the lambda calculus, with added `constant`, `conditional`, `primapp` and `letrec`.

$$
\begin{align*}
e\ &\mathtt{::=\ } \mathtt{C} & \texttt{[constant]}
\\
&\mathtt{|\ \ \ \ \ } x & \texttt{[variable]}
\\
&\mathtt{|\ \ \ \ \ } (\mathtt{if}\ e\ e\ e) & \texttt{[conditional]}
\\
&\mathtt{|\ \ \ \ \ } (\circ\ e\ e) & \texttt{[primapp]}
\\
&\mathtt{|\ \ \ \ \ } (e\ e) & \texttt{[application]}
\\
&\mathtt{|\ \ \ \ \ } (\lambda x.e) & \texttt{[lambda]}
\\
&\mathtt{|\ \ \ \ \ } (\mathtt{letrec}\ (b_0\dots b_n)\ e) & \texttt{[letrec]}
\\
b\ &\mathtt{::=\ } ( x:\ \lambda y.e ) & \texttt{[letrec binding]}
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
\mathcal{F}(\circ\ e_0\ e_1) &= \mathcal{F}e_0\cup \mathcal{F}e_1
\\
\mathcal{F}(e_0\ e_1) &= \mathcal{F}e_0\cup \mathcal{F}e_1
\\
\mathcal{F}(\lambda x.e) &= \mathcal{F}e - \set{ x }
\\
\mathcal{F}(\mathtt{letrec}\ (( x_0:\ \lambda_0)\dots( x_n:\ \lambda_n))\ e) &=
\Big( \mathcal{F}e\cup\bigcup_{i=0}^{i=n}\mathcal{F}\lambda_i\Big) - \set{x_0\dots x_n}
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
\mathcal{S}[x/e](\circ\ e_0\ e_1) &= (\circ\ \mathcal{S}[x/e]e_0\ \mathcal{S}[x/e]e_1)
\\
\mathcal{S}[x/e](e_0\ e_1) &= (\mathcal{S}[x/e]e_0\ \mathcal{S}[x/e]e_1)
\\
\mathcal{S}[x/e](\lambda y.e_0) &= \begin{cases}
(\lambda y.e_0) &\text{if }x = y
\\
(\lambda y.\mathcal{S}[x/e]e_0)&\text{otherwise}
\end{cases}
\\
\mathcal{S}[x/e](\mathtt{letrec}\ (b_0\dots b_n)\ e) &=
(\mathtt{letrec}\ (\mathcal{S}[x/e]b_0\dots \mathcal{S}[x/e]b_n)\ \mathcal{S}[x/e]e)
\\
\mathcal{S}[x/e]( y:\ \lambda z.e) &= \begin{cases} ( y:\ \lambda z.\mathcal{S}[x/e]e) &\text{if } x \not \in \set{y,z}
\\
( y:\ \lambda z.e) &\text{otherwise}
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
\beta (\circ\ e_0\ e_1) &= (\circ\ \beta e_0\ \beta e_1)
\\
\beta((\lambda x.e_0)\ e_1) &= \mathcal{S}[x/\beta e_1]\beta e_0
\\
\beta (e_0\ e_1) &= (\beta e_0\ \beta e_1)
\\
\beta (\lambda x.e) &= (\lambda x . \beta e)
\\
\beta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\beta b_0 \dots \beta b_n)\ \beta e)
\\
\beta ( x:\ \lambda y.e ) &= ( x:\  \lambda y . \beta e )
\end{align*}
$$

## Eta Reduction $\eta$

$\lambda x.(e x) = e$, basically.

$$
\begin{align*}
\eta\mathtt{C} &= \mathtt{C}
\\
\eta x &= x
\\
\eta (\mathtt{if}\ e_0\ e_1\ e_2) &= (\mathtt{if}\ \eta e_0\  \eta e_1\ e_2)
\\
\eta (\circ\ e_0\ e_1) &= (\circ\ \eta e_0\  \eta e_1)
\\
\eta (e_0\ e_1) &= (\eta e_0\  \eta e_1)
\\
\eta(\lambda x.(e\ x)) &= \begin{cases}\eta e &\text{iff } x \not \in \mathcal{F}e
\\
(\lambda x .\eta(e\ x)) &\text{otherwise}
\end{cases}
\\
\eta(\lambda x.e) &= (\lambda x.\eta e)
\\
\eta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\eta b_0 \dots \eta b_n)\ \eta e)
\\
\eta ( x:\ \lambda y.e ) &= ( x:\  \lambda y . \eta e )
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
\mathcal{T}(\circ\ e_0\ e_1) &= (\circ\ \mathcal{T}e_0\ \mathcal{T}e_1)
\\
\mathcal{T}(e_0\ e_1) &= (\mathcal{T}e_0\ \mathcal{T}e_1)
\\
\mathcal{T}(\lambda x . e) &= (\lambda x.\mathcal{T}e)
\\
L &=
\mathcal{T}(\mathtt{letrec}\ (( x_0:\ \lambda_0)\dots ( x_n:\ \lambda_n))\ e) &=
(\mathtt{letrec}\ (\set{(x_j:\ \mathcal{T}\lambda_j) | x_j \in
\bigcup_{i=0}^{i=n}\mathcal{F}\mathcal{T}\lambda_i \cup \mathcal{F}\mathcal{T}e
})\ \mathcal{T}e)
\\
\mathcal{T}(\mathtt{letrec}\ (\ )\ e) &= \mathcal{T}e
\end{align*}
$$

## Inline $\mathcal{I}$

A function is safe to inline if:

1. it is below a (tunable) size limit.
2. it is not recursive.
3. it only occurs once, and that occurrence is at a call site.

Once a function $x$ is determined to be safe, inlining is just $\mathcal{S}[x/\lambda y.e]$.

$$
\begin{align*}
\mathcal{I}\mathtt{C} &= \mathtt{C}
\\
\mathcal{I}x &= x
\\
\mathcal{I}(\mathtt{if}\ e_0\ e_1\ e_2) &=
    (\mathtt{if}\ \mathcal{I}e_0\ \mathcal{I}e_1\ \mathcal{I}e_2)
\\
\mathcal{I}(\circ\ e_0\ e_1) &= (\circ\ \mathcal{I}e_0\ \mathcal{I}e_1)
\\
\mathcal{I}(e_0\ e_1) &= (\mathcal{I}e_0\ \mathcal{I}e_1)
\\
\mathcal{I}(\lambda x.e) &= (\lambda x.\mathcal{I}e)
\\
\mathcal{I}(\mathtt{letrec}\ ((x_0:\ \lambda_0)\dots(x_n:\ \lambda_n))\ e) &=
    (\mathtt{letrec}\ ((x_0:\ \mathcal{S^{\ast}I}\lambda_0)\dots(x_n:\ \mathcal{S^{\ast}I}\lambda_n)\ \mathcal{S^{\ast}I}e)
\\
\text{where}
\\
\mathcal{S^{\ast}}y &= \mathcal{Scs}[x_i/\lambda_i]y\ \forall(x_i:\ \lambda_i) \in R
\\
R &= \set{(x_i:\ \lambda_i) \in \set{(x_0:\ \lambda_0)\dots(x_n:\ \lambda_n)}| \text{safe}\ \lambda_i}
\\
\text{safe}\ \lambda &= \mathcal{Z}\lambda < \mathtt{MAX} \land \lnot\mathcal{R}\lambda
\end{align*}
$$

Except the decision of whether to substitute based on number of occurrences is left to a variant of $\mathcal{S}$ $\mathcal{Scs}$.

### Size $\mathcal{Z}$

This is totally arbitrary, though more refined approaches may exist.

$$
\begin{align*}
\mathcal{Z}\mathtt{C} &= 1
\\
\mathcal{Z}x &= 1
\\
\mathcal{Z}(\mathtt{if}\ e_0\ e_1\ e_2) &= 1 + \mathcal{Z}e_0 + \mathcal{Z}e_1 + \mathcal{Z}e_2
\\
\mathcal{Z}(\circ\ e_0\ e_1) &= 1 + \mathcal{Z}e_0 + \mathcal{Z}e_1
\\
\mathcal{Z}(e_0\ e_1) &= 1 + \mathcal{Z}e_0 + \mathcal{Z}e_1
\\
\mathcal{Z}(\lambda x.e) &= 1 + \mathcal{Z}e
\\
\mathcal{Z}(\mathtt{letrec}\ (b_0\dots b_n)\ e) &= 1 + \sum_{i=0}^{i=n}\mathcal{Z}b_i + \mathcal{Z}e
\\
\mathcal{Z}(x:\ \lambda y.e) &= 1+ \mathcal{Z}e
\end{align*}
$$

### Recursive $\mathcal{R}$

Only `letrec` can bind recursive functions, and even mutual recursion is limited
to the bindings of a single `letrec`.

So given:

$$
\begin{align*}
l &= (\mathtt{letrec}\ (b_0\dots b_n)\ e)
\\
B &= \set{b_0\dots b_n} = \set{(x_0:\ \lambda y_0.e_0)\dots(x_n:\ \lambda y_n.e_n)}
\\
K &= \set{x_0\dots x_n}
\\
M &= \set{x_i \mapsto x_j | x_j \in \mathcal{F}(\lambda y_i.e_i) \cap K}\ \forall (x_i:\ \lambda y_i.e_i)\in B
\\
x &\in K
\end{align*}
$$

Then

$$
\mathcal{R}x = (x \mapsto x) \in M^+
$$

($M^+$ is the transitive closure of $M$).

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
\mathcal{C}_x(\circ\ e_0\ e_1) &= \mathcal{C}_xe_0 + \mathcal{C}_xe_1
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
\mathcal{C}_x(\mathtt{letrec}\ (b_0\dots b_n)\ e) &= \mathcal{C}_x e + \sum_{i=0}^{i=n}\mathcal{C}_x b_i
\\
\mathcal{C}_x(y:\ \lambda z.e) &= \begin{cases}
0 & \text{if } x = y \text{ (shadowing)}
\\
\mathcal{C}_x\lambda z.e &\text{otherwise}
\end{cases}
\end{align*}
$$
