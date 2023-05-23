# Type Checking

My notes on an absolutely fantastic YouTube series by [Adam Jones](https://www.youtube.com/@adam-jones).

## Lambda Calculus Grammar

$$
\begin{align}
\mathtt{e} &= \mathtt{x} & \mathtt{[variable]}
\\
& |\ \mathtt{e_1}\ \mathtt{e_2} & \mathtt{[application]}
\\
& |\ \lambda \mathtt{x} \rightarrow \mathtt{e} & \mathtt{[abstraction]}
\\
& |\ \mathtt{let\ x = e_1\ in\ e_2} & \mathtt{[let]}
\end{align}
$$

You might think that the $\mathtt{[let]}$ term is redundant, because $\mathtt{let\ x = e_1\ in\ e_2}$
is synonymous with $(\lambda x.e_2)e_1$ but $\mathtt{let}$ is treated differently by the type-checking
algorithm, as a way to achieve polymorphism.

## Free Variables in Expressions

Free variables are variables that have no value (free as in "not bound").
We can define a free variables function
$\mathcal{FV}$ for lambda expressions as

$$
\begin{align}
\mathcal{FV}(\mathtt{x}) &= \set{\mathtt{x}} & \mathtt{[variable]}
\\
\mathcal{FV}(\mathtt{e_1}\ \mathtt{e_2}) &= \mathcal{FV}(\mathtt{e_1}) \cup \mathcal{FV}(\mathtt{e_2}) & \mathtt{[application]}
\\
\mathcal{FV}(\lambda \mathtt{x} \rightarrow \mathtt{e}) &= \mathcal{FV}(\mathtt{e}) - \set{x} & \mathtt{[abstraction]}
\\
\mathcal{FV}(\mathtt{let\ x = e_1\ in\ e_2})  &= \mathcal{FV}(\mathtt{e_1}) \cup \mathcal{FV}(\mathtt{e_2}) - \set{x} & \mathtt{[let]}
\end{align}
$$

## Hindley Milner Type Grammar

$$
\begin{align}
\tau &= \alpha & \mathtt{[variable]}
\\
& |\ C \tau_1 \dots \tau_n & \mathtt{[application]}
\\
\\
\sigma &= \tau & \mathtt{[monotype]}
\\
& |\ \forall \alpha . \sigma & \mathtt{[quantifier]}
\end{align}
$$

There are two parts.
* $\tau$ is a **monotype**, which can be a siple type $\alpha$ like `int` or `string`, or
it can be a "type function application" $C$ like $\mathtt{List[}\tau\mathtt{]}$ or $\tau_1 \rightarrow \tau_2$.
$\mathtt{Bool}$, $\mathtt{Int}$ etc are also type function applications, but with no arguments.
* $\sigma$ is a **polytype**
which can be either a monotype $\tau$ or a quantified polytype $\forall\alpha.\sigma$. Quantified polytypes
are how we deal with polymorphic functions. More on that lter, but essentially $\forall\alpha$ is saying
"any $\alpha$ in the subsequent expression is local to it".

Example

$$
\forall \alpha . \mathtt{List}(\alpha \rightarrow \alpha)
$$

## Type Assignment

We represent type assignment as for example

$$
\mathtt{e}:\sigma
$$

Which just says the expression $\mathtt{e}$ has the type $\sigma$

## Contexts

contexts store type assignments. The usual symbol for a context is

$$
\Gamma
$$

Contexts behave like linked lists of assignments. Another way to think of
contexts is as "type environments".

We can state that an assignment is in a context as follows

$$
\mathtt{age}:\mathtt{Int} \in \Gamma
$$

## Context Grammar 

The grammar for contexts is simple

$$
\begin{align}
\Gamma &= \emptyset & \mathtt{[empty]}
\\
& |\ \Gamma, \mathtt{e}:\sigma & \mathtt{[extended]}
\end{align}
$$

## Typing Judgements

Given a context and an expression, we can make typing judgements about the expression.
Typing Judgements look like this

$$
\Gamma\vdash \mathtt{e}:\sigma
$$

Which can be read as "from the context $\Gamma$ it follows ($\vdash$) that $e$ has type $\sigma$."

## Typing Rules

A typing rule looks like

$$
{\mathtt{x}:\sigma \in \Gamma \above{2pt} \Gamma \vdash \mathtt{x}:\sigma}
$$

Where the upper part is called the premise, and the lower part the conclusion or judgement.

You can read this example as "**if** the assignment $\mathtt{x}:\sigma$ is in the context $\Gamma$ **then**
from the context $\Gamma$ it follows that $\mathtt{x}$ has type $\sigma$." This is almost a tautology,
but a necessary one when specifying a type checking algorithm.

## Free Variables in Type Expressions

Analogous to free variables in expressions, we can define a function to calculate the free variables in a type expression:

$$
\begin{align}
\mathcal{FV}(\alpha) &= \set{\alpha} & \mathtt{[variable]}
\\
\mathcal{FV}(C \tau_1 \dots \tau_n) &= \mathcal{FV}(\tau_1) \cup \dots \cup \mathcal{FV}(\tau_n) & \mathtt{[application]}
\\
\mathcal{FV}(\forall\alpha. \sigma) &= \mathcal{FV}(\sigma) - \set{\alpha} & \mathtt{[quantifier]}
\end{align}
$$

The interesting thing here is the quantifier rule which starts to hint at how polymorphism is handled: $\alpha$ is not free in
a polytype quantified by $\alpha$.

## Free Variables in Contexts

We can also describe a rule for free variables in a context, referring back to the Context Grammar above,

$$
\begin{align}
\mathcal{FV}(\emptyset) &= \set{} & \mathtt{[empty]}
\\
\mathcal{FV}(\Gamma, \mathtt{e}:\sigma) &= \mathcal{FV}(\Gamma) \cup \mathcal{FV}(\sigma) & \mathtt{[extended]}
\end{align}
$$

## Substitutions

Substitutions are sets of mappings from symbols to terms, where terms are general constructions of symbols,
like arithmetic expressions etc. Mappings are applied simultaneously.

For example

$$
S = \set{\mathtt{h} \mapsto \mathtt{l}, \mathtt{e} \mapsto \mathtt{a}, \mathtt{l} \mapsto \mathtt{s}}
$$

says $S$ is the substitution mapping $\mathtt{h}$ to $\mathtt{l}$ etc.

so if $\mathtt{h}$ etc. are characters, then

$$
S(\mathtt{hello})  = \mathtt{lasso}
$$

Note that $\mathtt{h}$ in $\mathtt{hello}$ went to $\mathtt{l}$, but was *not* subsequently mapped to $\mathtt{s}$.
That is what the "mappings are applied simultaneously" rule was about.

## Substitutions in Type Systems

Direct quote:

> Hindley-Milner type inference algorithms use substitutions from type variables to monotypes,
> applied on types.

Examples

$$
\begin{align}
S &= \set{\alpha \mapsto \beta, \beta \mapsto \mathtt{Int}}
\\
S(\alpha \rightarrow \beta) &= \beta \rightarrow \mathtt{Int}
\\
S(S(\alpha \rightarrow \beta)) &= \mathtt{Int} \rightarrow \mathtt{Int}
\end{align}
$$

## Combining (Composing) Substitutions

If we have more than one substitution, we can compose them into a single substitution.

Example

$$
\begin{align}
S_1 &= \set{\mathtt{h} \mapsto \mathtt{i}}
\\
S_2 &= \set{\mathtt{o} \mapsto \mathtt{h}}
\\
S_2(S_1(\mathtt{oh})) &= \mathtt{hi}
\\
S_3 &= \set{\mathtt{h} \mapsto \mathtt{i}, \mathtt{o} \mapsto \mathtt{h}}
\\
S_3(\mathtt{oh}) &= \mathtt{hi}
\end{align}
$$

In this case the composition of the two substitutions is just their union $S_1 \cup S_2$.

However

$$
\begin{align}
S_1 &= \set{\mathtt{h} \mapsto \mathtt{i}}
\\
S_2 &= \set{\mathtt{o} \mapsto \mathtt{h}}
\\
S_1 (S_2 (\mathtt{oh}) ) &= \mathtt{ii}
\\
S_3 &= \set{\mathtt{h} \mapsto \mathtt{i}, \mathtt{o} \mapsto \mathtt{i}}
\\
S_3(\mathtt{oh}) &= \mathtt{ii}
\end{align}
$$

$S_3$ is different, so the order of application matters.

A semi-manual way to calculate the composition of two substitutions like this is
to draw up a table. The leftmost column is the symbols being mapped from,
intermediate columns are the results of applying a substitution on the previous
column, and the final column is the resulting mapping:

|              | $S_2$        | $S_1$        | $S_3$                           |
|--------------|--------------|--------------|---------------------------------|
| $\mathtt{h}$ | $\mathtt{i}$ | $\mathtt{i}$ | $\mathtt{h} \mapsto \mathtt{i}$ |
| $\mathtt{o}$ | $\mathtt{h}$ | $\mathtt{i}$ | $\mathtt{o} \mapsto \mathtt{i}$ |

The set of the mappings in column $S_3$ is the final substitution:
$\set{\mathtt{h} \mapsto \mathtt{i}, \mathtt{o} \mapsto \mathtt{i}}$

## Unifying Substitutions

Another direct quote:

> A substitution unifies two values if, when applid to both, the results are equal.

$$
S(a) = S(b)
$$

Example

$$
\begin{align}
S &= \set{ \mathtt{r} \mapsto \mathtt{y}, \mathtt{s} \mapsto \mathtt{y}, \mathtt{d} \mapsto \mathtt{s} }
\\
a &= \mathtt{red}
\\
b &= \mathtt{yes}
\\
S(a) &= \mathtt{yes}
\\
S(b) &= \mathtt{yey}
\end{align}
$$

so $S$ does not unify $a$ and $b$.

We can also ask "what substitution unifies $a$ and $b$?" There are obviously many possible
substitutions in this case.

The substitution with the fewest mappings is called the "Most General Unifying Solution".

If we rely on the fact that substitutions always map from symbols, this sometimes restricts the possible solutions,
for example

$$
\begin{align}
a &= 3 + (7 \times z)
\\
b &= y + (x \times 2)
\\
S &= \set{ y \mapsto 3, x \mapsto 7, z \mapsto 2 }
\end{align}
$$

another example

$$
\begin{align}
a &= 2 + 3
\\
b &= y
\\
S &= \set { y \mapsto 2 + 3 }
\end{align}
$$

Just demonstrates that the expressions in a substitution can be complex.

Another example

$$
\begin{align}
a &= 3 \times 7
\\
b &= 3 + z
\end{align}
$$

In this case there is no unifying solution.

Another example

$$
\begin{align}
a &= 1 + z
\\
b &= z
\\
S &= \set{ z \mapsto 1 + 1 + 1 + \dots }
\end{align}
$$

This is a solution, but it's not ok, attempting to unify a variable with an
expression that it occurs in results in an infinite expansion which might be
ok mathematically, but it's no use to a type checking algorithm.

Anyway this all gives us a signature for the unification functon which I'm calling $\mathcal{U}$:

$$
\begin{align}
S &= \mathcal{U}(a, b)
\\
S(a) &= S(b)
\end{align}
$$

## Applying Unification to Type Systems

Example

$$
\begin{align}
a &= \mathtt{Int} \rightarrow \alpha
\\
b &= \beta \rightarrow \mathtt{Bool}
\\
S &= \mathcal{U}(a, b)
\\
S &= \set{ \alpha \mapsto \mathtt{Bool}, \beta \mapsto \mathtt{Int} }
\end{align}
$$

Another example

$$
\begin{align}
a &= \mathtt{List}\ \alpha
\\
b &= \beta \rightarrow \mathtt{Bool}
\\
S &= \mathcal{U}(a, b)
\end{align}
$$

Has no solutions as the structure is different.

Another example

$$
\begin{align}
a &= \alpha \rightarrow \mathtt{Int}
\\
b &= \alpha
\\
S &= \mathcal{U}(a, b)
\\
S &= \set{\alpha \mapsto \mathtt{Int} \rightarrow \mathtt{Int} \rightarrow \mathtt{Int} \rightarrow \dots}
\end{align}
$$

This is the "occurs in" error again, $\alpha$ is a function with an infinite number of arguments.

## Unification Algorithm for HM Types

```plaintext
unify(a: Monotype, b: Monotype) -> Substitution:
  if a is a type variable:
    if b is the same type variable:
      return {}
    if b contains a:
      throw "Error occurs check"
    return { a -> b }
  if b is a type variable:
    return unify(b, a)
  if a and b are both type function applications:
    if a and b have different type functions:
      throw "Error unification failed"
    S = {}
    for i in range(number of type function arguments):
      S = combine(S, unify(a.args[i], b.args[i])
    return S
```

Just remember that we're talking about type function applications here, not lambdas, i.e. if the type
function application is $\mathtt{List}\ \beta$ then the arguments are $\beta$, or if the type
function application is $\mathtt{Int}\rightarrow\alpha$ then the arguments are $\mathtt{Int}$ and $\alpha$,
and if the type function application is $\mathtt{Int}$ then there are no arguments.

## Type Order

> Some types are more general than others.

Consider

$$
\forall \alpha . \forall \beta . \alpha \rightarrow \beta
$$

There is a sense in which the above is "more general" than something like this:

$$
\forall \alpha . \alpha \rightarrow \mathtt{Bool}
$$

which in turn is more genral that something like

$$
\mathtt{Int} \rightarrow \mathtt{Bool}
$$

This can be represented by the "strictly more general than" $\sqsubset$ relation:

$$
\forall \alpha . \alpha \rightarrow \mathtt{Bool} \sqsubset \mathtt{Int} \rightarrow \mathtt{Bool}
$$

There is also a less strict "more general than" $\sqsubseteq$ operator, kind of like a "less than or equal to"

$$
\begin{align}
\alpha & \sqsubseteq \alpha
\\
\forall \alpha . \alpha & \sqsubseteq \mathtt{Int}
\end{align}
$$

and in fact $\forall\alpha.\alpha$ is like zero in this relation, it's the most general possible type expression
because it means any type variable.

### Formal Definition of Type Order
> $\sigma_1$ is more general than $\sigma_2$ if there is a substitution $S$ that maps the for-all quantified
> variables in $\sigma_1$, and $S(\sigma_1) = \sigma_2$.

Example

$$
\begin{align}
\forall \alpha . \alpha \rightarrow \mathtt{Bool} &\sqsubseteq \mathtt{Int} \rightarrow \mathtt{Bool}
\\
S &= \set{\alpha \mapsto \mathtt{Int}}
\\
S(\forall \alpha . \alpha \rightarrow \mathtt{Bool}) &= \mathtt{Int} \rightarrow \mathtt{Bool}
\end{align}
$$
