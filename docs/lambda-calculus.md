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
&\mathtt{|\ \ \ \ \ } \lambda x.e & \texttt{[lambda]}
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
\mathcal{F}(\lambda x.e) &= \mathcal{F}e - \set{ x } && \text{(1)}
\\
\mathcal{F}(\mathtt{letrec}\ (( x_0:\ \lambda_0)\dots( x_n:\ \lambda_n))\ e) &=
\Big( \mathcal{F}e\cup\bigcup_{i=0}^{i=n}\mathcal{F}\lambda_i\Big) - \set{x_0\dots x_n} && \text{(2)}
\end{align*}
$$

1. The free variables in a lambda are the free variables in its body minus its argument.
2. The free variables in a `letrec` are the free variables in its lambdas (1) plus the free variables in its body, minus the variables bound by the letrec itself.

## Substitution $\mathcal{S}_{[x/r]}$

Substitution only replaces free variables, and is understood to be capture-avoiding: before descending under a binder, alpha-rename any bound variable that would otherwise capture a free variable of the replacement term. This is a rather subtle point: if $r$ contains a variable that is bound by a scope it is substituting within, that would produce an unsound result.

Write $e_0[y'/y]_{\alpha}$ for the result of alpha-renaming the bound occurrences of $y$ in $e_0$ to a fresh variable $y'$.

$$
\begin{align*}
\mathcal{S}_{[x/r]}\mathtt{C} &= \mathtt{C}
\\
\mathcal{S}_{[x/r]}y &= \begin{cases}
r &\text{if } x = y
\\
y &\text{otherwise}
\end{cases}
\\
\mathcal{S}_{[x/r]}(\mathtt{if}\ e_0\ e_1\ e_2) &= (\mathtt{if}\ \mathcal{S}_{[x/r]}e_0\ \mathcal{S}_{[x/r]}e_1\ \mathcal{S}_{[x/r]}e_2)
\\
\mathcal{S}_{[x/r]}(\circ\ e_0\ e_1) &= (\circ\ \mathcal{S}_{[x/r]}e_0\ \mathcal{S}_{[x/r]}e_1)
\\
\mathcal{S}_{[x/r]}(e_0\ e_1) &= (\mathcal{S}_{[x/r]}e_0\ \mathcal{S}_{[x/r]}e_1)
\\
\mathcal{S}_{[x/r]}(\lambda y.e_0) &=
\bigl\{
\begin{aligned}
(\lambda y.e_0) &\quad \text{if } x = y && \text{(1)}
\\
(\lambda y.\mathcal{S}_{[x/r]}e_0) &\quad \text{if } y \notin \mathcal{F}r
\\
(\lambda y'.\mathcal{S}_{[x/r]}(e_0[y'/y]_{\alpha})) &\quad \text{otherwise, where } y' \notin \mathcal{F}r \cup \mathcal{F}e_0 \cup \set{x}
\end{aligned}
\bigr.
\\
l &= (\mathtt{letrec}\ B\ e)
\\
\mathcal{S}_{[x/r]}l &=
\bigl\{
\begin{aligned}
l &\quad \text{if } x \in K && \text{(2)}
\\
l' &\quad \text{if } (K \cup Z) \cap \mathcal{F}r = \set{}
\\
\widehat{l}' &\quad \text{otherwise} && \text{(3)}
\end{aligned}
\bigr.
\end{align*}
$$

where

$$
\begin{align*}
B &= ((y_i:\ \lambda z_i.e_i))_{i=0}^n
\\
K &= \set{y_i \mid 0 \le i \le n}
\\
Z &= \set{z_i \mid 0 \le i \le n}
\\
B' &= ((y_i:\ \lambda z_i.\mathcal{S}_{[x/r]}e_i))_{i=0}^n
\\
\widehat{B}' &= ((y'_i:\ \lambda z'_i.\mathcal{S}_{[x/r]}\widehat{e}_i))_{i=0}^n
\\
e' &= \mathcal{S}_{[x/r]}e
\\
\widehat{e}' &= \mathcal{S}_{[x/r]}\widehat{e}
\\
l' &= (\mathtt{letrec}\ B'\ e')
\\
\widehat{l}' &= (\mathtt{letrec}\ \widehat{B}'\ \widehat{e}')
\end{align*}
$$

1. If $y$ shadows $x$, substitution stops at that lambda.
2. A letrec-bound name shadows $x$ throughout every binding and the letrec body.
3. $\widehat{e}_0,\dots,\widehat{e}_n,\widehat{e}$ are obtained by alpha-renaming the letrec-bound names $y_i$ and the formal parameters $z_i$ consistently to fresh names $y'_i, z'_i$, with every $y'_i, z'_i \notin \mathcal{F}r$.

For inlining we also use a call-site-only variant $\mathcal{S}^{cs}_{[x/r]}$, defined by

$$
\begin{align*}
\mathcal{S}^{cs}_{[x/r]}y &= y
\\
\mathcal{S}^{cs}_{[x/r]}(e_0\ e_1) &=
\bigl\{
\begin{aligned}
(r\ \mathcal{S}^{cs}_{[x/r]}e_1) &\quad \text{if } e_0 = x && \text{(4)}
\\
(\mathcal{S}^{cs}_{[x/r]}e_0\ \mathcal{S}^{cs}_{[x/r]}e_1) &\quad \text{otherwise}
\end{aligned}
\bigr.
\end{align*}
$$

All other clauses of $\mathcal{S}^{cs}$, including the same alpha-renaming side conditions, are the same as $\mathcal{S}$.

In (4), this variant only replaces occurrences of $x$ in function position.

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
\beta((\lambda x.e_0)\ e_1) &= \mathcal{S}_{[x/\beta e_1]}\beta e_0 && \text{(1)}
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

1. everything else is navigation, this is the only substitution $\mathcal{S}$
in the specific case where the function is an anonymous lambda.

## Eta Reduction $\eta$

$\lambda x.(e x) = e$, basically.

$$
\begin{align*}
\eta\mathtt{C} &= \mathtt{C}
\\
\eta x &= x
\\
\eta (\mathtt{if}\ e_0\ e_1\ e_2) &= (\mathtt{if}\ \eta e_0\ \eta e_1\ \eta e_2)
\\
\eta (\circ\ e_0\ e_1) &= (\circ\ \eta e_0\  \eta e_1)
\\
\eta (e_0\ e_1) &= (\eta e_0\  \eta e_1)
\\
\eta(\lambda x.(e\ x)) &=
\bigl\{
\begin{aligned}
\eta e &\quad \text{iff } x \not \in \mathcal{F}e && \text{(1)}
\\
(\lambda x .\eta(e\ x)) &\quad \text{otherwise}
\end{aligned}
\bigr.
\\
\eta(\lambda x.e) &= (\lambda x.\eta e)
\\
\eta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\eta b_0 \dots \eta b_n)\ \eta e)
\\
\eta ( x:\ \lambda y.e ) &= ( x:\  \lambda y . \eta e )
\end{align*}
$$

1. The wrinkle is that we can only do the reduction if $x$ is not free in $e$.

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
l &= (\mathtt{letrec}\ (( x_0:\ \lambda_0)\dots ( x_n:\ \lambda_n))\ e) && \text{(1)}
\\
K &= \set{x_0\dots x_n} && \text{(2)}
\\
\vec{D} &= \set{x_i \mapsto \  \set{x_j\dots x_k} | x_i \in K,\ x_j\dots x_k \in K \cap \mathcal{F}\mathcal{T}\lambda_i} && \text{(3)}
\\
B &= \mathcal{F}\mathcal{T}e \cap K && \text{(4)}
\\
L &= \bigcup_{x\in B} \vec{D}^{\ast}(x) && \text{(5)}
\\
\mathcal{T}l &=
(\mathtt{letrec}\ (\set{(x_i:\ \mathcal{T}\lambda_i) | x_i \in L})\ \mathcal{T}e)
\\
\mathcal{T}(\mathtt{letrec}\ (\ )\ e) &= \mathcal{T}e
\end{align*}
$$

The preliminaries are just navigating to the `letrec`. Having got there:

1. Let $l$ be a `letrec` with keys $x_0\dots x_n$, lambdas $\lambda_0\dots\lambda_n$ and body $e$.
2. Let $K$ be the set of just the keys of $l$.
3. Let $\vec{D}$ be the relation of each $x_i$ in $K$ to a set of other $\set{x_j\dots x_k}$ in $K$ where $x_j\dots x_k$ are free in $x_i$'s associated tree-shook lambda $\mathcal{T}\lambda_i$. Essentially a map from the lambda name to any letrec bound variables in its body.
4. Let $B$ be the set of elements of $K$ that are free in the tree-shook body $\mathcal{T}e$.
5. Let $L$ be the set of live variables: those $x_i$ in $K$ that are reachable from $e$ via zero or more applications of $\vec{D}$.

Then $\mathcal{T}l$ is the tree-shook letrec $l$, With bindings restricted to those whose keys are members of $L$.

Finally, though not properly part of tree shaking,
if the result is a `letrec` with no bindings it reduces
to just the tree-shook body.

## Inline $\mathcal{I}$

A function is safe to inline if:

1. it is below a (tunable) size limit.
2. it is not recursive.
3. it only occurs once, and that occurrence is at a call site.

Once a function $x$ is determined to be safe, inlining is just $\mathcal{S}^{cs}_{[x/\lambda y.e]}$.

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
l &= (\mathtt{letrec}\ ((x_0:\ \lambda_0)\dots(x_n:\ \lambda_n))\ e)
\\
\mathcal{I}l &= (\mathtt{letrec}\ ((x_0:\ \mathcal{S}_{cs}^{\ast}\mathcal{I}\lambda_0)\dots(x_n:\ \mathcal{S}_{cs}^{\ast}\mathcal{I}\lambda_n))\ \mathcal{S}_{cs}^{\ast}\mathcal{I}e)
\\
\text{where}
\\
\mathcal{S}_{cs}^{\ast}y &= \mathcal{S}^{cs}_{[x_0/\lambda_0]}\dots\mathcal{S}^{cs}_{[x_m/\lambda_m]}y
\\
R &= \set{(x_i:\ \lambda_i) \in \set{(x_0:\ \lambda_0)\dots(x_n:\ \lambda_n)} | \operatorname{safe}(x_i, \lambda_i, l)} = \set{(x_0:\ \lambda_0)\dots(x_m:\ \lambda_m)}
\\
\operatorname{safe}(x_i, \lambda_i, l) &= \mathcal{Z}\lambda_i < \mathtt{MAX} \land \lnot\mathcal{R}x_i \land \mathcal{C}_{x_i}l = 1 \land \mathcal{C}^{cs}_{x_i}l = 1
\end{align*}
$$

Except the decision of whether to substitute based on number and position of occurrences is left to the call-site substitution variant $\mathcal{S}^{cs}$.

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

Count all free occurrences of $x$, or only occurrences at call sites.

$$
\begin{align*}
\mathcal{C}_x\mathtt{C} &= 0
\\
\mathcal{C}_xy &=\begin{cases}
1 &\text{if } x = y
\\
0 &\text{otherwise}
\end{cases}
\\
\mathcal{C}_x(\mathtt{if}\ e_0\ e_1\ e_2) &= \mathcal{C}_xe_0 +\mathcal{C}_xe_1 +  \mathcal{C}_xe_2
\\
\mathcal{C}_x(\circ\ e_0\ e_1) &= \mathcal{C}_xe_0 + \mathcal{C}_xe_1
\\
\mathcal{C}_x(e_0\ e_1) &= \mathcal{C}_xe_0 + \mathcal{C}_xe_1
\\
\mathcal{C}_x(\lambda y.e) &= \begin{cases}
0 & \text{if } x = y\text{ (shadowing)}
\\
\mathcal{C}_x e &\text{otherwise}
\end{cases}
\\
\mathcal{C}_x(\mathtt{letrec}\ ((y_0:\ \lambda z_0.e_0)\dots(y_n:\ \lambda z_n.e_n))\ e) &= \begin{cases}
0 & \text{if } x \in \set{y_0\dots y_n}
\\
\mathcal{C}_x e + \sum_{i=0}^{i=n}\mathcal{C}_x(\lambda z_i.e_i) &\text{otherwise}
\end{cases}
\end{align*}
$$

For the call-site-only count used by inlining, define $\mathcal{C}^{cs}_x$ by

$$
\begin{align*}
\mathcal{C}^{cs}_xy &= 0
\\
\mathcal{C}^{cs}_x(e_0\ e_1) &= \begin{cases}
1 + \mathcal{C}^{cs}_x e_1 &\text{if } e_0 = x
\\
\mathcal{C}^{cs}_x e_0 + \mathcal{C}^{cs}_x e_1 &\text{otherwise}
\end{cases}
\end{align*}
$$

All other clauses of $\mathcal{C}^{cs}$ are the same as $\mathcal{C}$.
