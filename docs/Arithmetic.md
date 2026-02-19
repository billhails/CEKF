# Arithmetic

Notes to self on the equations of rational and complex arithmetic.

## Domains

For future reference:

* Natural numbers $\mathbb{N}$, $\{1,2,\cdots\}$
* Integers $\mathbb{Z}$, $\{\cdots-1,0,1\cdots\}$
* Rationals $\mathbb{Q}$
* Gaussian (complex) integers $\mathbb{Z}[i]$
* Gaussian rationals $\mathbb{Q}[i]$
* Real numbers $\mathbb{R}$
* Gaussian numbers $\mathbb{C}$

## Rational Arithmetic

### addition

$$
\frac{a}{b} + \frac{c}{d} = \frac{(ad + bc)}{bd}
$$

### subtraction

$$
\frac{a}{b} - \frac{c}{d} = \frac{(ad - bc)}{bd}
$$

### multiplication

$$
\frac{a}{b} \times \frac{c}{d} = \frac{ac}{bd}
$$

### division

$$
\frac{a}{b} \div \frac{c}{d} = \frac{ad}{bc}
$$

### power

$$
\Big(\frac{a}{b}\Big)^n = \frac{a^n}{b^n}
$$

### modulus

$$
\frac{a}{b}\mod{\frac{c}{d}} = \frac{ad\mod{bc}}{bd}
$$

### comparison

$$
\frac{a}{b}\lesseqgtr{\frac{c}{d}} = ad\lesseqgtr{bc}
$$

## Complex Number Arithmetic

### complex addition

$$
(a+bi)+(c+di) = (a+c) + (b + d)i
$$

### complex subtraction

$$
(a+bi)-(c+di) = (a-c)+(b-d)i
$$

### complex multiplication

$$
(a+bi)(c + di) = (ac - bd)+(ad+bc)i
$$

### complex division

$$
\frac{a+bi}{c+di} = \frac{(ac + bd) + (bc -ad)i}{c^2+d^2}
$$

### complex/integer power

$$
\begin{align*}
(a+bi)^n &= \displaystyle\sum_{x=1}^n\binom{n}{x}a^{n-x}bi^x
\\
\text{where } \binom{n}{x} &= \frac{n!}{x!(n-x)!}
\end{align*}
$$

### complex root

$$
\begin{align*}
\sqrt[n]{a + bi} &= \sqrt[n]{r(\cos\theta + i\sin\theta)}&\text{convert to polar coordinates:}
\\
&&r = \sqrt{a^2 + b^2}
\\
&&\theta = \arctan(b/a)
\\
&= \sqrt[n]{r} \Bigg( \cos\frac{\theta + 2\pi k}{n} + i \sin \frac{\theta + 2\pi k}{n} \Bigg) &\text{for }k \in \{0 \dots n-1\}
\\
&= \sqrt[n]{r} \Bigg( \cos\frac{\theta}{n} + i \sin \frac{\theta}{n} \Bigg) &\text{for }k = 0
\end{align*}
$$

### integer/complex power

$$
c^{a + bi} = c^a[\cos (b \cdot \ln c) + i \cdot \sin(b \cdot \ln c)]
$$

### complex raised to complex power

$$
\begin{align*}
(a+bi)^{c+di} && \text{convert to polar co-ordinates}
\\
&& r = \sqrt{a^2 + b^2}
\\
&&\theta = \arctan(b/a)
\\
&= e^{(\ln(r) + \theta i)\cdot(c+di)}
\end{align*}
$$

### Integer LCM

$$
\operatorname{lcm}(a,b) =
    \frac{\vert a \cdot b \vert}{\operatorname{gcd}(a,b)}
$$

### Rational GCD

$$
\operatorname{gcd}\Big(\frac{a}{b},\frac{c}{d}\Big) =
    \frac{\operatorname{gcd}(a,c)}{\operatorname{lcm}(b,d)}
$$

### Rational LCM

$$
\operatorname{lcm}\Big(\frac{a}{b}, \frac{c}{d}\Big) =
    \frac{\operatorname{lcm}(a,c)}{\operatorname{gcd}(b,d)}
$$

## Canonical GCD Extensions

This section defines a deterministic canonical gcd (`cgcd`) contract for
integer, rational, Gaussian integer, and Gaussian rational values.

The goal is to preserve useful non-trivial results while remaining mathematically
coherent and implementation-friendly.

### scope

* Supported domains for `cgcd`: $\mathbb{Z}$, $\mathbb{Q}$, $\mathbb{Z}[i]$,
  $\mathbb{Q}[i]$.
* Inputs outside those domains are invalid for `cgcd` (and generate `NaN`).
* `lcm` is defined only where needed by these formulas.

### core identities

$$
\operatorname{cgcd}(0,0)=0
$$

$$
\operatorname{cgcd}(x,0)=\operatorname{normalize}(x)
$$

$$
\operatorname{cgcd}(x,y)=\operatorname{cgcd}(y,x)
$$

### constant-folding invariants (integer domains)

These are the simplest invariants that are stable across integer-backed
domains used by the runtime (machine integers, big integers, and
unit-normalized Gaussian integers).

Let $\operatorname{canon}(x)$ mean canonical normalization for the domain:

* in $\mathbb{Z}$: sign-normalize ($\operatorname{canon}(x)=|x|$ when used as a gcd result)
* in $\mathbb{Z}[i]$: unit-normalize to the chosen principal associate

Then the following identities are safe fold targets:

#### symmetry and idempotence

$$
\operatorname{gcd}(a,b)=\operatorname{gcd}(b,a),\quad
\operatorname{lcm}(a,b)=\operatorname{lcm}(b,a)
$$

$$
\operatorname{gcd}(a,a)=\operatorname{canon}(a),\quad
\operatorname{lcm}(a,a)=\operatorname{canon}(a)
$$

#### zero identities

$$
\operatorname{gcd}(a,0)=\operatorname{canon}(a),\quad
\operatorname{gcd}(0,a)=\operatorname{canon}(a)
$$

$$
\operatorname{lcm}(a,0)=0,\quad
\operatorname{lcm}(0,a)=0
$$

#### one and unit behavior

In $\mathbb{Z}$:

$$
\operatorname{gcd}(a,1)=1,\quad
\operatorname{lcm}(a,1)=\operatorname{canon}(a)
$$

In $\mathbb{Z}[i]$, replace $1$ with any unit; after normalization, gcd is the
canonical unit and lcm is the normalized input associate.

#### absorption and product relation

For integer domains where divisibility is defined, and with canonical
normalization applied to outputs:

$$
\operatorname{gcd}(a,\operatorname{lcm}(a,b))=\operatorname{canon}(a)
$$

$$
\operatorname{lcm}(a,\operatorname{gcd}(a,b))=\operatorname{canon}(a)
$$

$$
\operatorname{canon}\big(\operatorname{gcd}(a,b)\cdot\operatorname{lcm}(a,b)\big)=\operatorname{canon}(a\cdot b)
$$

For $\mathbb{Z}[i]$, this is equality up to multiplication by a unit before
normalization.

### canonical normal forms

#### integers ($\mathbb{Z}$)

* Canonical integer gcd is non-negative.

#### rationals ($\mathbb{Q}$)

* Store each rational as $a/b$ with:
  * $b>0$.
  * $\gcd(|a|,b)=1$.
* Canonical sign is carried by the numerator only.

#### Gaussian integers ($\mathbb{Z}[i]$)

* Units are $\{\pm1,\pm i\}$.
* Any gcd is defined up to multiplication by a unit.
* Canonical associate is chosen by the unit-normalization rule below.

#### Gaussian rationals ($\mathbb{Q}[i]$)

* Real and imaginary components are reduced rationals with positive
    denominators.
* After arithmetic operations, normalize both components to reduced
    rational form.

### unit-normalization rule (deterministic)

For non-zero $z\in\mathbb{Z}[i]$ or $z\in\mathbb{Q}[i]$, consider
$\{z,-z,iz,-iz\}$. Choose exactly one representative by this priority:

1. Prefer $\Re(z)>0$.
2. If $\Re(z)=0$, prefer $\Im(z)>0$.
3. If more than one candidate still satisfies (possible on axes), choose the
     one with largest $\Re(z)$, then largest $\Im(z)$.

For $z=0$, return $0$.

### integer and rational definitions

#### integer gcd

$$
\operatorname{cgcd}_{\mathbb{Z}}(a,b)=\gcd(|a|,|b|)
$$

#### integer lcm

$$
\operatorname{lcm}(a,b)=\begin{cases}
0 & \text{if }a=0\text{ or }b=0 \\
\frac{|ab|}{\gcd(|a|,|b|)} & \text{otherwise}
\end{cases}
$$

#### rational gcd

For canonical $a/b$ and $c/d$:

$$
\operatorname{cgcd}_{\mathbb{Q}}\Big(\frac{a}{b},\frac{c}{d}\Big)=
\frac{\gcd(|a|,|c|)}{\operatorname{lcm}(b,d)}
$$

#### rational lcm

$$
\operatorname{lcm}_{\mathbb{Q}}\Big(\frac{a}{b},\frac{c}{d}\Big)=
\frac{\operatorname{lcm}(|a|,|c|)}{\gcd(b,d)}
$$

### Gaussian integer gcd over $\mathbb{Z}[i]$

Given $\alpha,\beta\in\mathbb{Z}[i]$ and $\beta\neq0$:

$$
q = \operatorname{round}(\Re(\alpha/\beta)) + i\operatorname{round}(\Im(\alpha/\beta))
$$

Rounding rule must be deterministic:

* Round to nearest integer in each component.
* On exact half ties, round away from zero.

$$
r = \alpha - \beta q
$$

Iterate Euclid on $(\beta, r)$ using norm $N(a+bi)=a^2+b^2$ until remainder is
zero.

Algorithm:

1. $(u,v)\leftarrow(\alpha,\beta)$.
2. While $v\neq0$:
    * $q\leftarrow\operatorname{round}(u/v)$ component-wise.
    * $r\leftarrow u-vq$.
    * $(u,v)\leftarrow(v,r)$.
3. Return `normalize_unit(u)`.

### Gaussian rational gcd over $\mathbb{Q}[i]$

Lift to $\mathbb{Z}[i]$, compute gcd there, then scale back.

1. Choose $D\in\mathbb{Z}_{>0}$ that clears all component denominators.
2. Compute $X=Dx$, $Y=Dy$ in $\mathbb{Z}[i]$.
3. Compute $G=\operatorname{cgcd}_{\mathbb{Z}[i]}(X,Y)$.
4. Return $g=G/D$.
5. Reduce rational real/imag parts and apply `normalize_unit`.

`D` must be deterministic:

* Let each rational component be in reduced form.
* `D` is the lcm of all positive denominators appearing in both inputs.

### determinism requirements

* No branch may depend on platform-specific floating tie behavior.
* Unit normalization is always applied at the end of $\mathbb{Z}[i]$ and
    $\mathbb{Q}[i]$ gcd computations.
* Rational denominators are always normalized to positive values.

## GCD/LCM Test Checklist

| Domain | Case | Expected property |
| --- | --- | --- |
| $\mathbb{Z}$ | $(6,4)$ | $\operatorname{gcd}=2$ |
| $\mathbb{Z}$ | $(0,9)$ | $\operatorname{gcd}=9$ |
| $\mathbb{Q}$ | $(3/2,9/4)$ | $\operatorname{gcd}=3/4$ |
| $\mathbb{Q}$ | $(2/3,4/9)$ | $\operatorname{gcd}=2/9$ |
| $\mathbb{Z}[i]$ | associates | gcd canonicalized to one unit representative |
| $\mathbb{Z}[i]$ | Euclid step | $N(r)<N(\beta)$ after rounded quotient |
| $\mathbb{Q}[i]$ | denominator lift | clear denominators, gcd in $\mathbb{Z}[i]$, scale back |
