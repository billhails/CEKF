# Type Checking

Or, "Climbing the Hindley-Milner Mountain" 😁

My notes on an absolutely fantastic YouTube series by [Adam Jones](https://www.youtube.com/@adam-jones).
These are initially for my own benefit, explaining it to myself, so if you don't follow go watch the videos.

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

> These two types are *not interchangeable*. pay careful attention to where we use a $\tau$ and where we use a $\sigma$
> in subsequent equations.

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

## Instantiation of Types in HM

Referring back to the [Unification algorithm](#unification-algorithm-for-hm-types), note that it only accepts monotypes, it can't
deal with $\forall$ quantified types.

Instantiation allows us to get from polytypes to monotypes.

Example

$$
\begin{align}
S &= \set{ \alpha \mapsto \mathtt{Int}, \beta \mapsto \mathtt{List}\ \gamma}
\\
S(\forall \alpha . \forall \beta . \alpha \rightarrow \beta) &= \mathtt{Int} \rightarrow \mathtt{List}\ \gamma
\end{align}
$$

> If $\sigma_1 \sqsubseteq \sigma_2$ then an expression of type $\sigma_2$ can be used where one of type $\sigma_1$ is needed.

## Generalisation of types in HM

Referring back to [Free Variables in Type Expressions](#free-variables-in-type-expressions) we know

$$
\mathcal{FV}(\forall \alpha . \alpha \rightarrow \beta) = \alpha
$$

We can use this to get from specific types to more general types by quantifying the free variables in an expression:

$$
\forall \beta . \alpha \rightarrow \beta \sqsupset \forall \alpha . \forall \beta . \alpha \rightarrow \beta
$$

That is essentially what generalization is: adding a $\forall$ quantifier to a free type variable in a type.
In HM we can only generalize a type when the type variable is not free in the context, so the signature
for generalize, $\mathcal{G}$ is

$$
\mathcal{G}(\Gamma, \sigma) = \textup{the most generalized version of the type }\sigma
$$

Example

$$
\begin{align}
\Gamma &= \mathtt{x}:\beta,\quad \mathtt{y}:\mathtt{List}\ \gamma \rightarrow \mathtt{Int},\quad \mathtt{z}:\forall \delta . \delta
\\
\sigma &= \forall \epsilon . \alpha \rightarrow \beta \rightarrow \gamma \rightarrow \delta \rightarrow \epsilon
\\
\mathcal{G}(\Gamma, \sigma) &= ?
\\
\mathcal{FV}(\Gamma) &= \set{\beta, \gamma} & \textup{not }\delta\textup{ because it is }\forall\textup{ quantified}
\\
\mathcal{FV}(\sigma) &= \set{\alpha, \beta, \gamma, \delta} & \textup{not }\epsilon\textup{ for the same reason}
\\
\mathcal{FV}(\sigma) - \mathcal{FV}(\Gamma) &= \set{ \alpha, \delta }
\\
\mathcal{G}(\Gamma, \sigma) &= \forall \alpha . \forall \delta . \forall \epsilon . \alpha \rightarrow \beta \rightarrow \gamma \rightarrow \delta \rightarrow \epsilon
\end{align}
$$

## Typing Judgements

Given a context and an expression, we can make typing judgements about the expression.
Typing Judgements look like this

$$
\Gamma\vdash \mathtt{e}:\sigma
$$

Which can be read as "from the context $\Gamma$ it follows ($\vdash$) that $e$ has type $\sigma$."

## HM Typing Rules

A typing rule looks like

$$
{\mathtt{x}:\sigma \in \Gamma \above{1pt} \Gamma \vdash \mathtt{x}:\sigma}
$$

Where the upper part is called the premise, and the lower part the conclusion or judgement.

You can read this example as "**if** the assignment $\mathtt{x}:\sigma$ is in the context $\Gamma$ **then**
from the context $\Gamma$ it follows that $\mathtt{x}$ has type $\sigma$." This is almost a tautology,
but a necessary one when specifying a type checking algorithm.

### The VAR Rule

The first typing rule in HM is $\mathtt{VAR}$ for "variable" and is the one we just looked at:

$$
{\mathtt{x}:\sigma \in \Gamma \above{1pt} \Gamma \vdash \mathtt{x}:\sigma}\qquad\mathtt{[VAR]}
$$

see [above](#typing-rules).

### The APP Rule

$\mathtt{APP}$ for "application".

$$
{\Gamma \vdash \mathtt{e_0}:\tau_a \rightarrow \tau_b\qquad \Gamma \vdash \mathtt{e_1}: \tau_a
\above{1pt}
\Gamma \vdash \mathtt{e_0}\ \mathtt{e_1}:\tau_b }\qquad\mathtt{[APP]}
$$

You can read this as **if** from the context it follows that $\mathtt{e_0}$ has type $\tau_a \rightarrow \tau_b$
**and** from the context $\mathtt{e_1}$ has type $\tau_a$ **then** from the context it follows that the
application of $\mathtt{e_0}$ to $\mathtt{e_1}$ has type $\tau_b$, or mor colloquially "the application
of a function of type $\tau_a \rightarrow \tau_b$ to a type $\tau_a$ results in a $\tau_b$." 

One point to note is that $\tau_a$ and $\tau_b$ are monotypes, this rule doesn't apply to polytypes ($\sigma$).

Concrete example

$$
{ 
  {
     {\mathtt{odd}:\mathtt{Int} \rightarrow \mathtt{Bool} \in \Gamma}
     \above{1pt}
     \Gamma \vdash \mathtt{odd}:\mathtt{Int} \rightarrow \mathtt{Bool}
  }
  \qquad
  {
     {\mathtt{age}: \mathtt{Int} \in \Gamma}
     \above{1pt}
     \Gamma \vdash \mathtt{age}: \mathtt{Int}
  }
\above{1pt}
  \Gamma \vdash \mathtt{odd}\ \mathtt{age}:\mathtt{Bool}
}
$$

This additionally shows stacking of rules to produce derivation trees.

### The ABS Rule

ABS for "abstraction".

$$
{
  \Gamma, \mathtt{x} : \tau_a \vdash \mathtt{e} : \tau_b
  \above{1pt}
  \Gamma \vdash \lambda \mathtt{x} \rightarrow \mathtt{e} : \tau_a \rightarrow \tau_b
}\qquad\mathtt{[ABS]}
$$

Says **if** from the context plus an assignment $\mathtt{x}$ has type $\tau_a$ it follows that $\mathtt{e}$ has type $\tau_b$,
**then** from the context it follows that expression $\lambda \mathtt{x} \rightarrow \mathtt{e}$ has type $\tau_a \rightarrow \tau_b$.

Example

$$
{
  \Gamma, \mathtt{n} : \mathtt{Int} \vdash \mathtt{gt\ 3\ n} : \mathtt{Bool}
  \above{1pt}
  \Gamma \vdash \lambda \mathtt{n} \rightarrow \mathtt{gt\ 3\ n} : \mathtt{Int} \rightarrow \mathtt{Bool}
}
$$

Where

$$
\Gamma = \mathtt{gt} : \mathtt{Int} \rightarrow \mathtt{Int} \rightarrow \mathtt{Bool}
$$

This is subtle, why the extra assigment outside of the context? and why from the context alone does it
follow that the function abstraction has that type? I think it's working backwards from the body of the
function abstraction to its argument type, and the $\mathtt{n}$ is only required as a placeholder for a
unifiable variable.

### The LET Rule

There is a special rule for $\mathtt{let}$ bindings.

$$
{
\Gamma \vdash \mathtt{e_0} : \sigma \qquad \Gamma, \mathtt{x} : \sigma \vdash \mathtt{e_1} : \tau
\above{1pt}
\Gamma \vdash \mathtt{let\ x = e_0\ in\ e_1} : \tau
}\qquad\mathtt{[LET]}
$$

Says that **if** from the context it follows that $\mathtt{e_0}$ has type $\mathtt{sigma}$ **and**
from the context plust a type assignment of $\sigma$ to $\mathtt{x}$ it follows that $\mathtt{e_1}$
has type $\tau$, **then** from the context alone it follows that $\mathtt{let\ x = e_0\ in\ e_1}$ has
type $\tau$.

Note that the result $\tau$ is constrained to be a monotype, this will be important later.
Also note that this is a kind of mixture of both the $\mathtt{APP}$ and $\mathtt{ABS}$ rules.

Example:

$$
{
\Gamma \vdash \mathtt{2} : \mathtt{Int} \qquad \Gamma, \mathtt{a} : \mathtt{Int} \vdash \mathtt{gt\ 3\ a} : \mathtt{Bool}
\above{1pt}
\Gamma \vdash \mathtt{let\ a = 2\ in\ gt\ 3\ a} : \mathtt{Bool}
}
$$

where

$$
\Gamma = \mathtt{gt} : \mathtt{Int} \rightarrow \mathtt{Int} \rightarrow \mathtt{Bool}, 2 : \mathtt{Int}
$$

In reality the context would have a generic way to assign $\mathtt{Int}$ to literal integers, rather than
enumerating them as here.

### The INST Rule

$\mathtt{INST}$ for "instantiation"

Refer back to [Instantiation of Types in HM](#instantiation-of-types-in-hm) for an introduction.

$$
{
\Gamma \vdash \mathtt{e} : \sigma_a \qquad \sigma_a \sqsubseteq \sigma_b
\above{1pt}
\Gamma \vdash \mathtt{e} : \sigma_b
}\qquad\mathtt{[INST]}
$$

Says **if** $\mathtt{e}$ has type $\sigma_a$ **and** $\sigma_a$ is more general than $\sigma_b$ **then**
$\mathtt{e}$ has (can have) type $\sigma_b$.

Example, given

$$
\Gamma = \mathtt{reverse}: \forall\alpha. \mathtt{List}\ \alpha \rightarrow \mathtt{List}\ \alpha, \mathtt{Ages} : \mathtt{List\ int}
$$

What is the type of $\mathtt{reverse\ ages}$?

$$
{
  {
    {
      \Gamma\ \vdash\ \mathtt{reverse}\ :\ \forall\alpha.\mathtt{List}\ \alpha \rightarrow \mathtt{List}\ \alpha
    }\ {\scriptsize\mathtt{[VAR]}}
    \qquad \forall\alpha.\mathtt{List}\ \alpha \rightarrow \mathtt{List}\ \alpha\ \sqsubseteq\ \mathtt{List\ Int} \rightarrow \mathtt{List\ Int}
    \above{1pt}
    \Gamma\ \vdash\ \mathtt{reverse} : \mathtt{List\ Int} \rightarrow \mathtt{List\ Int}
  }{\scriptsize\mathtt{[INST]}}
  \qquad
  {
    \Gamma \vdash \mathtt{ages}: \mathtt{List\ Int}
  }\ {\scriptsize\mathtt{[VAR]}}
  \above{1pt}
  \Gamma \vdash \mathtt{reverse\ ages} : \mathtt{List\ int}
}{\scriptsize\mathtt{[APP]}}
$$

### The GEN Rule

$\mathtt{GEN}$ for "generalization". Refer back to [Generalisation of Types in HM](#generalisation-of-types-in-hm)
for an introduction.

$$
{
\Gamma \vdash \mathtt{e} : \sigma \qquad \alpha \notin \mathcal{FV}(\Gamma)
\above{1pt}
\Gamma \vdash \mathtt{e} : \forall\alpha . \sigma
}\qquad\mathtt{[GEN]}
$$

Says **if** $\mathtt{e}$ has type $\sigma$ **and** $\alpha$ is not in the free variables of $\Gamma$
**then** the type of $\mathtt{e}$ is actually $\forall\alpha . \sigma$.

Partial example

$$
{
\Gamma \vdash \mathtt{things} : \mathtt{List}\ \alpha \qquad \alpha \notin \mathcal{FV}(\Gamma)
\above{1pt}
\Gamma \vdash \mathtt{things} : \forall\alpha . \mathtt{List}\ \alpha
}\qquad\mathtt{[GEN]}
$$

Says **if** $\mathtt{things}$ has type $\mathtt{List}\ \alpha$ **and** $\alpha$ is not in the free
variables of $\Gamma$ **then** $\forall$-quantify $\mathtt{List}\ \alpha$ with $\alpha$.

Concrete example

Given

$$
\Gamma = \mathtt{age} : \mathtt{Int}, \mathtt{odd} : \mathtt{Int} \rightarrow \mathtt{Bool}
$$

what is the type of $\mathtt{let\ id = \lambda x \rightarrow x\ in\ (id\ (odd\ (id\ age)))}$?

### Typing Proofs

Let's look at how we might automate the process.

If we want to derive the type of say $\mathtt{odd\ age}$ we start by givilg it a fresh type variable, say $t_0$:

$$
\Gamma \vdash \mathtt{odd\ age}: t_0
$$

Nw we know from it's structure that it's a function application, so we'll be using the $\mathtt{APP}$ rule, etc.

$$
{ 
  {
     {\mathtt{odd}:t_4 \in \Gamma}
     \above{1pt}
     \Gamma \vdash \mathtt{odd}:t_1 \rightarrow t_2
  }{\scriptsize\mathtt{[VAR]}}
  \qquad
  {
     {\mathtt{age}: t_5 \in \Gamma}
     \above{1pt}
     \Gamma \vdash \mathtt{age}: t_3
  }{\scriptsize\mathtt{[VAR]}}
\above{1pt}
  \Gamma \vdash \mathtt{odd}\ \mathtt{age}:t_0
}
{\scriptsize\mathtt{[APP]}}
$$

again we give the rule fresh variables.

Having gotten a set of variables, we can use the rules to work out how they should unify:

$$
\begin{align}
t_0 &\sim t_2
\\
t_1 &\sim t_3
\\
t_1 \rightarrow t_2 &\sim t_4
\\
t_4 &\sim \mathtt{Int} \rightarrow \mathtt{Bool}
\\
t_3 &\sim t_5
\\
t_5 &\sim \mathtt{Int}
\end{align}
$$

Once we have this list, we can discard the tree and just look at the final expression

$$
\Gamma \vdash \mathtt{odd}\ \mathtt{age}:t_0
$$

taking the first unification $t_0 \sim t_2$ and applying it, we get two possible unifications:

$$
\begin{align}
S = \mathcal{U}(t_0, t_2) &= \set{ t_0 \mapsto t_2 }
\\
                          &= \set{ t_2 \mapsto t_0 }
\end{align}
$$

These are actually equivalent, though it's tricky to explain how, but we need the first one.

if we apply that substitution to all the components, only one actually changes:

$$
S(\mathtt{odd}\ \mathtt{age}:t_0) = \mathtt{odd}\ \mathtt{age}:t_2
$$

we remove the constraint from our list and we're left with:

$$
\begin{align}
\Gamma &\vdash \mathtt{odd}\ \mathtt{age}:t_2
\\
t_1 &\sim t_3
\\
t_1 \rightarrow t_2 &\sim t_4
\\
t_4 &\sim \mathtt{Int} \rightarrow \mathtt{Bool}
\\
t_3 &\sim t_5
\\
t_5 &\sim \mathtt{Int}
\end{align}
$$

The next substitution applies to one of the unifications, not to the final statement

$$
\begin{align}
S &= \mathcal{U}(t_1, t_3)
\\
 &= \set{ t_1 \mapsto t_3 }
\end{align}
$$

We apply the sybstitution and discard the constraint

$$
S(t_1 \rightarrow t_2 \sim t_4) = t_3 \rightarrow t_2 \sim t_4
$$

resulting in 

$$
\begin{align}
\Gamma &\vdash \mathtt{odd}\ \mathtt{age}:t_2
\\
t_3 \rightarrow t_2 &\sim t_4
\\
t_4 &\sim \mathtt{Int} \rightarrow \mathtt{Bool}
\\
t_3 &\sim t_5
\\
t_5 &\sim \mathtt{Int}
\end{align}
$$

the next substitution

$$
\begin{align}
S &= \mathcal{U}(t_3 \rightarrow t_2, t_4)
\\
 &= \set{t_4 \mapsto (t_3 \rightarrow t_2)}
\end{align}
$$

apply and discard

$$
S(t_4 \sim \mathtt{Int} \rightarrow \mathtt{Bool}) = t_3 \rightarrow t_2 \sim \mathtt{Int} \rightarrow \mathtt{Bool}
$$

resulting in

$$
\begin{align}
\Gamma &\vdash \mathtt{odd}\ \mathtt{age}:t_2
\\
t_3 \rightarrow t_2 &\sim \mathtt{Int} \rightarrow \mathtt{Bool}
\\
t_3 &\sim t_5
\\
t_5 &\sim \mathtt{Int}
\end{align}
$$

The next substitution

$$
\begin{align}
S &= \mathcal{U}(t_3 \rightarrow t_2, \mathtt{Int} \rightarrow \mathtt{Bool})
\\
  &= \set{t_3 \mapsto \mathtt{Int}, t_2 \mapsto \mathtt{Bool}}
\end{align}
$$

Applying that

$$
\begin{align}
S(\mathtt{odd}\ \mathtt{age}:t_2) &= \mathtt{odd}\ \mathtt{age}:\mathtt{Bool}
\\
S(t_3 \sim t_5) &= \mathtt{Int} \sim t_5
\end{align}
$$

gives us our solution

$$
\begin{align}
\Gamma &\vdash \mathtt{odd}\ \mathtt{age}:\mathtt{Bool}
\\
\mathtt{Int} &\sim t_5
\\
t_5 &\sim \mathtt{Int}
\end{align}
$$

however we haven't finished, as any subsequent failures would indicate a problem with the unification as a whole,
so we continue:

$$
\begin{align}
S &= \mathcal{U}(\mathtt{Int}, t_5)
\\
 &= \set{t_5 \mapsto \mathtt{Int}}
\end{align}
$$

and applying this last substitution

$$
S(t_5 \sim \mathtt{Int}) = \mathtt{Int} \sim \mathtt{Int}
$$

we are left with one final unification to perform

$$
\begin{align}
\Gamma &\vdash \mathtt{odd}\ \mathtt{age}:\mathtt{Bool}
\\
\mathtt{Int} &\sim \mathtt{Int}
\end{align}
$$

doing that results in an empty mapping so no substitutions to perform so we discard the constraint and we're done.

$$
\begin{align}
S &= \mathcal{U}(\mathtt{Int}, \mathtt{Int})
\\
 &= \emptyset
\\
\\
\Gamma &\vdash \mathtt{odd}\ \mathtt{age}:\mathtt{Bool}
\\
\end{align}
$$

## Another Perspective


The videos refer to [This Paper](https://dl.acm.org/doi/10.1145/291891.291892) by Lee and Yi, which uses a slightly different lambda calculus and formulation of the typing rules, so we start by reviewing those.

### lambda grammar

$$
\begin{align*}
\mathtt{e} &= \mathtt{()} & \mathtt{[constant]}
\\
& |\ \mathtt{x} & \mathtt{[variable]}
\\
& |\ \mathtt{e_1}\ \mathtt{e_2} & \mathtt{[application]}
\\
& |\ \lambda \mathtt{x} . \mathtt{e} & \mathtt{[abstraction]}
\\
& |\ \mathtt{let\ x = e_1\ in\ e_2} & \mathtt{[let]}
\\
& |\ \mathtt{fix\ f}\ \lambda \mathtt{x}.\mathtt{e} & \mathtt{[fix]}
\end{align*}
$$

Most notable is the addition of a $\mathtt{[fix]}$ construct, which I believe is the pure functional equivalent of `letrec`, but I don't pretend to understand it and I have an alternative approach for handling `letrec`.


### type grammar

$$
\begin{align*}
\tau &= \iota & \textup{constant type}
\\
& |\ \alpha & \textup{type variable}
\\
& |\ \tau \rightarrow \tau & \textup{function type}
\end{align*}
$$

### type scheme

$$
\sigma = \tau\ |\ \forall\vec{\alpha}.\tau 
$$

Type Schemes are what we've been calling polytypes.
The $\forall\vec{\alpha}$ here is just shorthand for $\forall\alpha_1.\forall\alpha_2.\forall\alpha_3\dots$

### type env

$$
\Gamma \in \mathtt{x} \xrightarrow{fin} \sigma
$$

is a mapping from lambda variables to type schemes (not sure what the $fin$ means, "finite"?).

### typing rules

$$
\Gamma \vdash \mathtt{()}: \iota\qquad \mathtt{[con]}
$$

Is just saying there's an implicit mapping from constants to their types ($\set{1\mapsto Int, 2\mapsto Int, \dots}$ etc.)

$$
{
\Gamma(\mathtt{x}) \succ \tau
\above{1pt}
\Gamma \vdash \mathtt{x} : \tau
}\qquad\mathtt{[var]}
$$

The $\succ$ (successor) relation is their equivalent for $\sqsubseteq$, and $\Gamma(\mathtt{x})$ just means the value of $\mathtt{x}$ in $\Gamma$.

$$
{
\Gamma + \mathtt{x} : \tau_1 \vdash \mathtt{e} : \tau_2
\above{1pt}
\Gamma \vdash \lambda\mathtt{x}.\mathtt{e} : \tau_1 \rightarrow \tau_2
} \qquad \mathtt{[fn]}
$$

This is the ABS rule we've seen already.

$$
{
\Gamma \vdash \mathtt{e_1}: \tau_1 \rightarrow \tau_2\qquad \Gamma \vdash \mathtt{e_2} : \tau_1
\above{1pt}
\Gamma \vdash \mathtt{e_1 e_2} : \tau_2
}\qquad \mathtt{[app]}
$$

Exactly the same as our APP rule.

$$
{
\Gamma \vdash \mathtt{e_1} : \tau_1 \qquad \Gamma + Clos_\Gamma(\tau_1) \vdash \mathtt{e_2} : \tau_2
\above{1pt}
\Gamma \vdash \mathtt{let\ x = e_1\ in\ e_2}: \tau_2
}\qquad \mathtt{[let]}
$$

Again like our LET rule, but the $Clos$ function is turning $\tau_1$ into a polytype.

$$
{
\Gamma + \mathtt{f} : \tau \vdash \lambda\mathtt{x}.\mathtt{e} : \tau
\above{1pt}
\Gamma \vdash \mathtt{fix\ f}\ \lambda \mathtt{x}.\mathtt{e} : \tau
} \qquad \mathtt{[fix]}
$$

We won't need this rule as we support `letrec`.


## Algorithm W

We're now ready to tackle the HM algoritms themselves, starting with algorithm $\mathcal{W}$.

The signature for $\mathcal{W}$ is

$$
\mathcal{W}\ \Gamma \times \mathtt{e} = S \times \sigma
$$

e.g. it takes a tuple of a context and an expression, and returns a tuple of a substitution and a type.


### constants

$$
\mathcal{W}(\Gamma, \mathtt{()}) = (\emptyset, \iota)
$$

Applying $\mathcal{W}$ to a constant returns the empty (identity) substitution and the type of that constant (it just knows).

### variables

$$
\begin{align}
\mathcal{W}(\Gamma, \mathtt{x}) &= (\emptyset, \{\vec{\beta}/\vec{\alpha}\}\tau)
\\
\textup{where}
\\
&\Gamma(\mathtt{x}) = \forall\vec{\alpha}.\tau
\\
&\textup{new }\vec{\beta}
\end{align}
$$

This is saying that applying $\mathcal{W}$ to a variable looks up the possibly quantified type of the variable in the context and returns it, with all of its quantifiers replaced with fresh type variables.

### abstraction

$$
\begin{align*}
\mathcal{W(\Gamma, \lambda\mathtt{x}.\mathtt{e})} &= (S_1, S_1(\beta \rightarrow \tau_1))
\\
\textup{where}
\\
& (S_1, \tau_1) = \mathcal{W}(\Gamma + \mathtt{x}:\beta, \mathtt{e})
\\
&\textup{new }\beta
\end{align*}
$$

Applying $\mathcal{W}$ to a lambda expression first creates a fresh type variable $\beta$, then evaluates the body of the lambda $\mathtt{e}$ with a context extended by a mapping from $\mathtt{x}$ to $\beta$.

It then returns the substitution from the result, plus a function application type $\beta \rightarrow \tau$ (where $\tau$ is the type from the result) but with the substitution from the result applied to it.

### application

$$
\begin{align*}
\mathcal{W}(\Gamma, \mathtt{e_1e_2}) &= (S_3S_2S_1, S_3\beta)
\\
\textup{where}
\\
& S_3 = \mathcal{U}(S_2\tau_1, \tau_2 \rightarrow \beta)
\\
&\textup{new }\beta
\\
& (S_2, \tau_2) = \mathcal{W}(S_1\Gamma, \mathtt{e_2})
\\
&(S_1, \tau_1) = \mathcal{W}(\Gamma, \mathtt{e_1})
\end{align*}
$$

So when applying $\mathcal{W}$ to a function application $\mathtt{e_1e_2}$, first (reading from bottom to top) calculate the substitution for and type of $\mathtt{e_1}$ in the current context: $(S_1, \tau_1)$. Next apply the substitution $S_1$ to the current context and use that new context to determine the substitution for, and type of $\mathtt{e_2}$: $(S_2, \tau_2)$. Then create a fresh type variable $\beta$ and a function application from the inferred type of $\mathtt{e_2}$ to that $\beta$, and unify that with the inferred type of $\mathtt{e_1}$ after applying the substitution $S_2$ to it. Finally return the combination of all the inferred substitutions plus the type resulting from applying $S_3$ to $\beta$.

However our language supports multiple arguments to functions, so I'm proposing:

$$
\begin{align*}
\mathcal{W}(\Gamma, \mathtt{e_0e_1\dots e_n}) &= (S'S_nS_{n-1}, S'\beta)
\\
\textup{where}
\\
& S' = \mathcal{U}(S_n\tau_{n-1}, \tau_{n-1} \rightarrow \beta)
\\
&\textup{new }\beta
\\
& (S_n, \tau_n) = \mathcal{W}(S_{n-1}\Gamma, \mathtt{e_n})
\\
&(S_{n-1}, \tau_{n-1}) = \mathcal{W}(\Gamma, \mathtt{e_0\dots e_{n-1}})
\end{align*}
$$

It's basically the same apart from some variable renaming, except in the first line where $\mathtt{e_0}$ is applied to $\mathtt{e_1\dots e_n}$ arguments, and in the last line, where it recurses on $\mathtt{e_0\dots e_{n-1}}$


### let

$$
\begin{align*}
\mathcal{W}(\Gamma, \mathtt{let\ x = e_1\ in\ e_2}) &= (S_2S_1, \tau_2)
\\
\textup{where}
\\
&(S_2, \tau_2) = \mathcal{W}(S_1\Gamma + \mathtt{x}: Clos_{S_1\Gamma}(\tau_1), \mathtt{e_2})
\\
&(S_1, \tau_1) = \mathcal{W}(\Gamma, \mathtt{e_1})
\end{align*}
$$

When applying $\mathcal{W}$ to a `let` expression: $\mathtt{let\ x = e_1\ in\ e_2}$, first use the current context to determine the substitution for, and type of $\mathtt{e_1}$: $(S_1, \tau_1)$.
Use that substitution to modify the context, and then extend the context with a mapping from $\mathtt{x}$ to the polytype version of the type $\tau_1$, and use that context to infer the substitution for, and type of $\mathtt{e_2}$: $(S_2, \tau_2)$.
Return the type of $\mathtt{e_2}$ plus the combination of the two substitutions.

### fix

We won't need this. My idea for typechecking `letrec` I think is ok:

When actually evaluating (with `eval`) a letrec we need to create an environment populated with
dummy variables, then replace those variables with their actual values as the letrec bindings are computed.
That way the functions in a letrec can "see" themselves and all their siblings when they actually execute.

Analogously when typechecking a letrec we create an extended context with each variable bound
to a fresh type variable. Those variables will be unified with the types of the letrec expressions appropriately.
