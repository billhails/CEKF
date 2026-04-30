# $\lambda_m$ Macro Calculus

Notes on the Herman-Wand 07 paper

$$
\begin{align*}
e \texttt{ ::= } & v & \texttt{[expressions]}
\\
\texttt{| } & \lambda v . e
\\
\texttt{| } & e\ e
\\
\texttt{| } & \texttt{let syntax } x = m \texttt{ in } e \texttt{ end}
\\
\texttt{| } & {op [ s ]}^\sigma & \texttt{[macro application]}

\\
\\

v \texttt{ ::= } & x & \texttt{[program variables]}
\\
\texttt{| } & ?a & \texttt{[pattern variables]}

\\
\\

op \texttt{ ::= } & v & \texttt{[macro operators]}
\\
\texttt{| } & m

\\
\\

m \texttt{ ::= } & \texttt{macro } p : \sigma \Rightarrow e & \text{(1)}

\\
\\

p \texttt{ ::= } & ?a & \texttt{[tree of pattern variables]}
\\
\texttt{| } & (\bar{p})

\\
\\

s \texttt{ ::= } & e & \texttt{[s-expressions]}
\\
\texttt{| } & op
\\
\texttt{| } & (\bar{s})

\end{align*}
$$

1. Macros contain a pattern $p$, a *shape type annotation* $\sigma$ and a template expression $e$.

## Tree Locations

Trees $t$ are leaves or sequences of trees:

$$
\begin{align*}
t \texttt{ ::= } & L
\\
\texttt{| } & (\bar{t})
\end{align*}
$$

Tree locations $\ell$ are numbered paths through a tree.

## Shape Types

$$
\begin{align*}
\tau \texttt{ ::= } & \textsf{\textbf{expr}}
\\
\texttt{| } & \sigma \rightarrow \textsf{\textbf{expr}}
\\
\\
\beta \texttt{ ::= } & \langle \ell \rangle
\\
\texttt{| } & \textsf{\textbf{expr}}^{\ell,\bar{\ell}}
\\
\\
\sigma \texttt{ ::= } & \tau
\\
\texttt{| } & \beta
\\
\texttt{| } & (\bar{\sigma})
\end{align*}
$$
