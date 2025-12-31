β-Redux

> Notes taken from [OpenDSA](https://opendsa.cs.vt.edu/OpenDSA/Books/PL/html/BetaReduction.html)

# Substitution

$$
b[\lang p \rang \rightarrow a]\quad\text{replace $\lang p \rang$ with $a$ in $b$}
$$

$\lang p\rang$ is always a variable (angle brackets denote variables throughout).

## Case 1. $b$ is a variable

$$
\lang b\rang[\lang p\rang \rightarrow a]
$$

### Case 1a. $\lang p\rang = \lang b\rang$

$$
\lang p\rang[\lang p\rang \rightarrow a] = a
$$

### Case 1b.  $\lang p\rang \ne \lang b\rang$

$$
\lang b\rang[\lang p\rang \rightarrow a] = \lang b\rang
$$

## Case 2. $b$ is a $\lambda$-abstraction

$$
(\lambda \lang x\rang.E)[\lang p\rang \rightarrow a]
$$

### Case 2a. $\lang p\rang = \lang x\rang$

$$
(\lambda \lang x\rang.E)[\lang x\rang \rightarrow a] = \lambda \lang x\rang.E
$$

### Case 2b. $\lang p\rang \ne \lang x\rang$ and $\lang x\rang$ does not occur free in $a$

$$
(\lambda \lang x\rang.E)[\lang p\rang \rightarrow a] =
\lambda \lang x\rang.(E[\lang p\rang \rightarrow a])
$$

### Case 2c. $\lang p\rang \ne \lang x\rang$ but $\lang x\rang$ occurs free in $a$

Cannot happen if $b$ has been $\alpha$-converted.

## Case 3. $b$ is an application

$$
(e_1\;e_2)[\lang p\rang \rightarrow a] =
(e_1[\lang p\rang \rightarrow a]\;e_2[\lang p\rang \rightarrow a])
$$

# $\beta$-Reduction

An expression to which $\beta$-reduction can be applied is called a $\beta$-redex. A $\beta$-redex is an application in which the first term is a function abstraction. For example:

$$
(\lambda x.(x\;v)\;(z\;(v\;u)))
$$

On the other hand, this:

$$
((\lambda x.(x\;v)\;y)\;(z\;(v\;u)))
$$

Is not a $\beta$-redex becuse the first term is a function application not an abstraction (although the application itself is a $\beta$-redex.)

## $\beta$-reduction is substitution

Having identified a $\beta$-redex $(\lambda x.E\;E')$:

$$
\beta(\lambda x.E\;E') = E[\lang x\rang \rightarrow E']
$$

## Reduction strategies

Consider
$$
\lgroup\lambda x.m\; \lgroup \lambda x.(x\;x)\;\lambda x.(x\;x)\rgroup\rgroup
$$
and
$$
\lgroup\lambda x.((x\;y)\;(y\;x))\;\lgroup\lambda w.(w\;w)\;z\rgroup\rgroup
$$

These contain two $\beta$-redexes each (delimited by square-ish braces.)

There are two orders in which we can evaluate, inner to outer (applicative order) or outer to inner (normal order).

### Applicative order

$$
\begin{align*}
\lgroup\lambda x.m\; &\lgroup \lambda x.(x\;x)\;\lambda x.(x\;x)\rgroup\rgroup\\
                                 &\lgroup\lambda x.(x\;x)\;\lambda x.(x\;x)\rgroup&\text{case 2b. non-terminating}
\end{align*}
$$
$$
\begin{align*}
\lgroup\lambda x.((x\;y)\;(y\;x))\;&\lgroup\lambda w.(w\;w)\;z\rgroup\rgroup
\\
\lgroup\lambda x.((x\;y)\;(y\;x))\;&(z\;z)\rgroup
\\
(((z\;z)\;y)\;(y\;(z\;z)))
\end{align*}
$$

### Normal order

$$
\begin{align*}
\lgroup\lambda x.&m\; \lgroup \lambda x.(x\;x)\;\lambda x.(x\;x)\rgroup\rgroup\\
                                 &m&\text{$x$ does not occur in $m$}
\end{align*}
$$
$$
\begin{align*}
\lgroup\lambda x.((x\;y)\;(y\;x))\;&\lgroup\lambda w.(w\;w)\;z\rgroup\rgroup
\\
((\lgroup\lambda w.(w\;w)\;z\rgroup\;y)\;(y\;\lgroup\lambda w.(w\;w)\;z\rgroup))
\\
(((z\;z)\;y)\;(y\;(z\;z)))
\end{align*}
$$

Note that where normal order got both answers, it had to evaluate $\lgroup\lambda w.(w\;w)\rgroup$ twice.
