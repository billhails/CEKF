# Arithmetic

Notes to self on the equations of rational and complex arithmetic.

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
