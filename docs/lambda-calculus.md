# Essential Lambda Calculus

Just a little digression, I think it's interesting
how much of the transformational logic in the pipeline
can be reduced to pure math on a lambda calculus.

## Our grammar

This is basically the lambda calculus, with added `constant` and `letrec`.

$$
\begin{align*}
e\ &\mathtt{::=\ } \mathtt{C} & \texttt{[constant]}
\\
&\mathtt{|\ \ \ \ \ } x & \texttt{[variable]}
\\
&\mathtt{|\ \ \ \ \ } (e\ e) & \texttt{[application]}
\\
&\mathtt{|\ \ \ \ \ } \lambda x.e & \texttt{[lambda]}
\\
&\mathtt{|\ \ \ \ \ } (\mathtt{letrec}\ (b_0\dots b_n)\ e) & \texttt{[letrec]}
\\
b\ &\mathtt{::=\ } ( x:\lambda y.e ) & \texttt{[letrec binding]}
\end{align*}
$$

The real `MinLam` core has other constructs but their transforms are obvious,
e.g. for an arbitrary transform $\mathcal{T}$ and container $\circ$,
$\mathcal{T}(\circ\ e_0\dots e_n) =$ either $(\circ\ \mathcal{T}e_0\dots\mathcal{T}e_n)$,
or $\bigoplus_{i = 0}^n\mathcal{T}e_i$ as appropriate.

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
\tag{1}\mathcal{F}\lambda x.e &= \mathcal{F}e - \set{ x }
\\
\tag{2}\mathcal{F}(\mathtt{letrec}\ (( x_0:\lambda_0)\dots( x_n:\lambda_n))\ e) &=
\Big( \mathcal{F}e\cup\bigcup_{i=0}^n\mathcal{F}\lambda_i\Big) - \set{x_0\dots x_n}
\end{align*}
$$

1. The free variables in a lambda are the free variables in its body minus its argument.
2. The free variables in a `letrec` are the free variables in its body plus the free variables in its lambdas (1), minus the variables bound by the letrec itself.

## Alpha Conversion $\alpha_{\rho}$

Alpha conversion renames binders to fresh names and rewrites bound occurrences to match.
It essentially guarantees that every semantically distinct variable has a different
name.

The implementation in `minlam_alphoconvert.c` is environment-based, so we specify it that way here. Let:

- $\rho$ be an environment mapping source names to their current alpha-converted names.
- $\rho[x \mapsto x']$ be the environment obtained by extending $\rho$ with a fresh binding for $x$.
- $\rho[x_0 \mapsto x'_0,\dots,x_n \mapsto x'_n]$ be the simultaneous extension used for `letrec`.
- $\mathrm{fresh}(x)$ be a globally fresh symbol derived from $x$.

The initial environment $\rho_0$ maps every built-in name to itself. Variable lookup always searches from the innermost environment outward. If a variable is not found, alpha conversion is undefined.

$$
\begin{align*}
\alpha_{\rho}\mathtt{C} &= \mathtt{C}
\\
\tag{1}
\alpha_{\rho}x &= \rho(x)

\\
\alpha_{\rho}(e_0\ e_1) &= (\alpha_{\rho}e_0\ \alpha_{\rho}e_1)

\\
\tag{2}
\alpha_{\rho}\lambda x.e &= \lambda x'.\alpha_{\rho[x \mapsto x']}e
& \text{where } & x' = \mathrm{fresh}(x)

\\
\tag{3}
l &= (\mathtt{letrec}\ (b_0\dots b_n)\ e)

\\
\rho' &= \rho[x_0 \mapsto x'_0,\dots,x_n \mapsto x'_n]
& \text{where } & b_i = (x_i:\lambda y_i.e_i),

\\
\tag{4}
&&&x'_i = \mathrm{fresh}(x_i)

\\
\tag{5}\alpha_{\rho}l &=
(\mathtt{letrec}\ (\alpha_{\rho'}b_0\dots\alpha_{\rho'}b_n)\ \alpha_{\rho'}e)

\\
\tag{6}\alpha_{\rho}(x:\lambda y.e) &= (\rho(x):\lambda y'.\alpha_{\rho[y \mapsto y']}e)
& \text{where } & y' = \mathrm{fresh}(y)
\end{align*}
$$

1. Every variable occurrence is rewritten by environment lookup. In the implementation, failure to find a mapping is an internal error.
2. A lambda allocates a fresh name for its argument, extends the environment, then visits the body in that extended environment.
3. For `letrec`, we first name the whole expression $l$ only to state the helper equations below it.
4. All `letrec` binding names are added to the environment before any binding body is visited. This is the crucial step that preserves mutual recursion and mirrors `visitLetRecVariables` in the implementation.
5. The `letrec` body is visited in the shared recursive environment $\rho'$, and each binding is alpha-converted in that same environment.
6. A binding keeps its already-chosen recursive name $\rho(x)$, then freshens only its formal argument before visiting the lambda body. This mirrors `visitLetRecValues` in the implementation.

## Substitution $\mathcal{S}_{[x/r]}$

Substitution only replaces free variables.

In general, substitution is capture-avoiding: if a binder in the target expression would capture a free variable of the replacement term $r$, that binder must first be alpha-renamed.

For the rest of this note we use the simpler convention that the input to substitution has already been alpha-converted enough to rule that out. Concretely, when forming $\mathcal{S}_{[x/r]}e$, assume no variable bound anywhere in $e$ appears free in $r$. Under that convention, substitution reduces to the following shadowing-only definition.

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
\mathcal{S}_{[x/r]}(e_0\ e_1) &= (\mathcal{S}_{[x/r]}e_0\ \mathcal{S}_{[x/r]}e_1)
\\
\tag{1}
\mathcal{S}_{[x/r]}\lambda y.e_0 &= \begin{cases}
\lambda y.e_0 &\text{if } x = y
\\
\lambda y.\mathcal{S}_{[x/r]}e_0 &\text{otherwise}
\end{cases}

\\
l &= (\mathtt{letrec}\ ((y_0:\lambda z_0.e_0)\dots(y_n:\lambda z_n.e_n))\ e)
\\
\tag{2}
\mathcal{S}_{[x/r]}l &= \begin{cases}
l &\text{if } x \in \set{y_0\dots y_n}
\\
(\mathtt{letrec}\ ((y_0:\lambda z_0.\mathcal{S}_{[x/r]}e_0)\dots
\\
\phantom{\mathtt{letrec}\ ((} (y_n:\lambda z_n.\mathcal{S}_{[x/r]}e_n))\ \mathcal{S}_{[x/r]}e) &\text{otherwise}
\end{cases}
\end{align*}
$$

1. If $y$ shadows $x$, substitution stops at that lambda.
2. A letrec-bound name shadows $x$ throughout every binding and the letrec body.

For inlining we also use a call-site-only variant $\mathcal{S}^{cs}_{[x/r]}$, defined by

$$
\begin{align*}
\mathcal{S}^{cs}_{[x/r]}y &= y
\\
\tag{3}
\mathcal{S}^{cs}_{[x/r]}(e_0\ e_1) &= \begin{cases}
(r\ \mathcal{S}^{cs}_{[x/r]}e_1) &\text{if } e_0 = x
\\
(\mathcal{S}^{cs}_{[x/r]}e_0\ \mathcal{S}^{cs}_{[x/r]}e_1) &\text{otherwise}
\end{cases}
\end{align*}
$$

All other clauses of $\mathcal{S}^{cs}$ are the same as $\mathcal{S}$, under the same alpha-converted freshness convention.

In (3), this variant only replaces occurrences of $x$ in function position.

## Beta Reduction $\beta$

Replace formal arguments with actual arguments in the body of an anonymous lambda application.

$$
\begin{align*}
\beta\mathtt{C} &= \mathtt{C}
\\
\beta x &= x
\\
\tag{1}
\beta((\lambda x.e_0)\ e_1) &= \mathcal{S}_{[x/\beta e_1]}\beta e_0
\\
\beta (e_0\ e_1) &= (\beta e_0\ \beta e_1)
\\
\beta \lambda x.e &= \lambda x . \beta e
\\
\beta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\beta b_0 \dots \beta b_n)\ \beta e)
\\
\beta ( x:\lambda y.e ) &= ( x: \lambda y . \beta e )
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
\eta (e_0\ e_1) &= (\eta e_0\  \eta e_1)
\\
\tag{1}
\eta\lambda x.(e\ x) &= \begin{cases}
\eta e &\text{iff } x \not \in \mathcal{F}e
\\
\lambda x .\eta(e\ x) &\text{otherwise}
\end{cases}
\\
\eta\lambda x.e &= \lambda x.\eta e
\\
\eta (\mathtt{letrec}\ (b_0\dots b_n)\  e) &= (\mathtt{letrec}\ (\eta b_0 \dots \eta b_n)\ \eta e)
\\
\eta ( x:\lambda y.e ) &= ( x: \lambda y . \eta e )
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
\mathcal{T}(e_0\ e_1) &= (\mathcal{T}e_0\ \mathcal{T}e_1)
\\
\mathcal{T}\lambda x . e &= \lambda x.\mathcal{T}e
\\
\tag{1}
l &= (\mathtt{letrec}\ (( x_0:\lambda_0)\dots ( x_n:\lambda_n))\ e)
\\
l' &= (\mathtt{letrec}\ (( x_0:\lambda'_0)\dots ( x_n:\lambda'_n))\ e')
\\
\tag{2}
&\phantom{=}\text{where }\lambda'_i = \mathcal{T}\lambda_i \text{ and }e' = \mathcal{T}e
\\
\tag{3}
K &= \set{x_0\dots x_n}
\\
\tag{4}
\vec{D} &= \set{ x_i \rightarrow x_j | x_i \in K,\ x_j \in K \cap \mathcal{F}\lambda'_i}
\\
\tag{5}
B &= \mathcal{F}e' \cap K
\\
\tag{6}
L &= \bigcup_{x\in B} \vec{D}^{\ast}x
\\
\tag{7}
\mathcal{T}l &=
(\mathtt{letrec}\ (\set{(x_i:\lambda'_i) | x_i \in L})\ e')
\\
\tag{8}
\mathcal{T}(\mathtt{letrec}\ (\ )\ e) &= \mathcal{T}e
\end{align*}
$$

The preliminaries are just navigating to the `letrec`. Having got there:

1. Let $l$ be a `letrec` with keys $x_0\dots x_n$, lambdas $\lambda_0\dots\lambda_n$ and body $e$.
2. Let $l'$ be the same `letrec` with $\mathcal{T}$ applied to $\lambda_0\dots\lambda_n$ and $e$, yielding equivalent $\lambda'_0\dots\lambda'_n$ and $e'$.
3. Let $K$ be the set of just the keys of $l$.
4. Let $\vec{D}$ be the set of mappings from each $x_i$ in $K$ to each $x_j$ in the free variables of $\lambda'_i$. Essentially a map from the lambda name to any letrec bound variables in its body.
5. Let $B$ be the set of elements of $K$ that are free in $e'$.
6. Let $L$ be the set of live variables: those $x_i$ in $K$ that are reachable from $B$ via zero or more applications of $\vec{D}$.
7. Then $\mathcal{T}l$ is $l'$, With bindings restricted to those whose keys are in $L$.
8. Finally, though not properly part of tree shaking,
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
\mathcal{I}(e_0\ e_1) &= (\mathcal{I}e_0\ \mathcal{I}e_1)
\\
\mathcal{I}\lambda x.e &= \lambda x.\mathcal{I}e
\\
l &= (\mathtt{letrec}\ ((x_0:\lambda_0)\dots(x_n:\lambda_n))\ e)
\\
\mathcal{I}l &= (\mathtt{letrec}\ ((x_0:\mathcal{S}_{cs}^{\ast}\mathcal{I}\lambda_0)\dots(x_n:\mathcal{S}_{cs}^{\ast}\mathcal{I}\lambda_n))\ \mathcal{S}_{cs}^{\ast}\mathcal{I}e)
\\
\text{where}
\\
\mathcal{S}_{cs}^{\ast}y &= \mathcal{S}^{cs}_{[x_0/\lambda_0]}\dots\mathcal{S}^{cs}_{[x_m/\lambda_m]}y
\\
R &= \set{(x_i:\lambda_i) \in \set{(x_0:\lambda_0)\dots(x_n:\lambda_n)} | \mathrm{safe}(x_i, \lambda_i, l)}
\\
&= \set{(x_0:\lambda_0)\dots(x_m:\lambda_m)}
\\
\mathrm{safe}(x_i, \lambda_i, l) &= \mathcal{Z}\lambda_i < \mathtt{MAX} \land \lnot\mathcal{R}x_i \land \mathcal{C}_{x_i}l = 1 \land \mathcal{C}^{cs}_{x_i}l = 1
\end{align*}
$$

Except the decision of whether to substitute based on number and position of occurrences is left to the call-site substitution variant $\mathcal{S}^{cs}$.

### Size $\mathcal{Z}$

This is totally arbitrary, much more refined approaches probably exist.

$$
\begin{align*}
\mathcal{Z}\mathtt{C} &= 1
\\
\mathcal{Z}x &= 1
\\
\mathcal{Z}(e_0\ e_1) &= 1 + \mathcal{Z}e_0 + \mathcal{Z}e_1
\\
\mathcal{Z}\lambda x.e &= 1 + \mathcal{Z}e
\\
\mathcal{Z}(\mathtt{letrec}\ (b_0\dots b_n)\ e) &= 1 + \mathcal{Z}e + \textstyle\sum_{i=0}^n\mathcal{Z}b_i
\\
\mathcal{Z}b &= 1+ \mathcal{Z}e
\\
&\phantom{=}\text{where } b = (x:\lambda y.e)
\end{align*}
$$

### Recursive $\mathcal{R}$

Only `letrec` can bind recursive functions, and mutual recursion is limited
to the bindings within a single `letrec`.

So:

$$
\begin{align*}
\text{given}
\\
\tag{1}
l &= (\mathtt{letrec}\ ((x_0:\lambda_0)\dots(x_n:\lambda_n))\ e)
\\
\tag{2}
K &= \set{x_0\dots x_n}
\\
\tag{3}
\vec{D} &= \set{x_i \mapsto x_j | x_i \in K,\ x_j \in K \cap \mathcal{F}\lambda_i}
\\
\tag{4}
x &\in K
\\
\text{then}
\\
\tag{5}
\mathcal{R}x &= (x \mapsto x) \in \vec{D}^+
\end{align*}
$$

1. let $l$ be a `letrec` with keys $x_0\dots x_n$, lambdas $\lambda_0\dots\lambda_n$ and body $e$.
2. let $K$ be the set of just the keys of $l$.
3. let $\vec{D}$ be the set of mappings from $x_i$ in $K$ to $x_j$ in $K$ where $x_j$ is also free in $x_i$'s associated $\lambda_i$.
4. let $x$ be in $K$.
5. then $x$ is recursive if it can reach itself by one or more applications of $\vec{D}$.

$\vec{D}^+$ is the transitive closure of $\vec{D}$. Note we've met $\vec{D}$
before in the Tree Shaking algorithm, but in that case we employed the reflexive
transitive closure $\vec{D}^\ast$ meaning zero or more applications.

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
\mathcal{C}_x(e_0\ e_1) &= \mathcal{C}_xe_0 + \mathcal{C}_xe_1
\\
\mathcal{C}_x\lambda y.e &= \begin{cases}
0 & \text{if } x = y\text{ (shadowing)}
\\
\mathcal{C}_x e &\text{otherwise}
\end{cases}
\\
\mathcal{C}_x(\mathtt{letrec}\ ((y_0:\lambda z_0.e_0)\dots(y_n:\lambda z_n.e_n))\ e) &= \begin{cases}
0 & \text{if } x \in \set{y_0\dots y_n}
\\
\mathcal{C}_x e + \sum_{i=0}^{n}\mathcal{C}_x(\lambda z_i.e_i) &\text{otherwise}
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
