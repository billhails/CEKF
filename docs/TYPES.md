# Type Checking

My notes on an absolutely fantastic YouTube series by [Adam Jones](https://www.youtube.com/@adam-jones).

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
are how we deal with polymorphic functions.

Example

$$
\forall \alpha . \mathtt{List}(\alpha \rightarrow \alpha)
$$

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

## Free Variables in Expressions

Free variables are variables that have no value (free as in "not bound").
Referring back to the lambda calculus grammar we can define a free variables function
$\mathcal{FV}$ for expressions as

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

