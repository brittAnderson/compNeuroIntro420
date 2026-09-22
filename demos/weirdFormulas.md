
### Basic Numbers & Arithmetic

#### 1. The Number 0

$$\lim_{x \to \infty} \left(1 + \frac{1}{x}\right)^x - \sum_{k=0}^{\infty} \frac{1}{k!}$$

* **Translation:** $0$
* **The De-obfuscation:** Both terms are just different definitions of Euler’s number ($e$). Subtracting $e - e$ gives $0$.

#### 2. The Number 1

$$\prod_{p \text{ prime}} \left(1 - \frac{1}{p^2}\right)^{-1} \cdot \frac{6}{\pi^2}$$

* **Translation:** $1$
* **The De-obfuscation:** The infinite product is Euler's solution to the Basel Problem ($\frac{\pi^2}{6}$). Multiplying $\frac{\pi^2}{6} \cdot \frac{6}{\pi^2}$ equals $1$.

#### 3. The Number 1 (Euler's Identity variant)

$$-\exp(i\pi)$$

* **Translation:** $1$
* **The De-obfuscation:** Euler's formula states $e^{i\pi} = -1$. Negating it gives $-(-1) = 1$.

#### 4. The Number 2 (Set Theory / Peano)

$$\vert{}\{\emptyset, \{\emptyset\}\}\vert{}$$

* **Translation:** $2$
* **The De-obfuscation:** In set theory (Von Neumann ordinals), numbers are defined by sets containing sets. This is a set containing two elements: the empty set, and a set with the empty set inside it. The cardinality $\vert{}\cdot\vert{}$ counts them: $2$.

#### 5. Simple Addition ($x + y$)

$$(x \oplus y) + 2(x \land y)$$

* **Translation:** $x + y$
* **The De-obfuscation:** This comes from Mixed Boolean Arithmetic (MBA). It states that $x + y$ equals the XOR sum (bits where they differ) plus twice the AND sum (the carry bits).

---

### Basic Operations & Functions

#### 6. Multiplying a Number by 2

$$\int_0^x 2 \, dt$$

* **Translation:** $2x$
* **The De-obfuscation:** Integrating the constant $2$ from $0$ to $x$ is just $2 \cdot x$.

#### 7. Taking the Average of Two Numbers ($a$ and $b$)

$$\arg\min_x \left( (x - a)^2 + (x - b)^2 \right)$$

* **Translation:** $\frac{a + b}{2}$ (The mid-point / average)
* **The De-obfuscation:** Find the point $x$ that minimizes the squared distance to $a$ and $b$. That point is always the exact middle.

#### 8. A Constant Function ($f(x) = 1$)

$$\frac{d}{dx}\left(x + \sin^2(x) + \cos^2(x)\right) - 1$$

* **Translation:** $0$
* **The De-obfuscation:** $\sin^2(x) + \cos^2(x) = 1$. So the expression is $\frac{d}{dx}(x + 1) - 1 = 1 - 1 = 0$.

#### 9. Absolute Value ($\vert{}x\vert{}$)

$$\sqrt{x^2}$$

* **Translation:** $\vert{}x\vert{}$ (Distance from zero)
* **The De-obfuscation:** Squaring any number makes it positive; taking the principal square root keeps it positive.

---

### Probability & Data (Neuro-Adjacent)

#### 10. Counting How Many Items Match a Condition

$$\sum_{i=1}^N \mathbb{I}(x_i > 0) \quad \text{where } \mathbb{I}(A) = \begin{cases} 1 & \text{if } A \text{ is true} \\ 0 & \text{otherwise} \end{cases}$$

* **Translation:** *"Count how many positive numbers are in the list."*
* **The De-obfuscation:** $\mathbb{I}$ is an "indicator function"—an `if` statement that returns $1$ or $0$. The summation $\sum$ acts as a counter loop.

#### 11. Normalizing Data into Percentages (Softmax)

$$p_i = \frac{e^{z_i}}{\sum_{j=1}^K e^{z_j}}$$

* **Translation:** *"Turn a set of scores into probabilities that add up to 100%."*
* **The De-obfuscation:** Exponentiating $e^{z_i}$ ensures all values are positive, and dividing by the sum forces the total to equal $1$.

#### 12. Normal Distribution / Bell Curve

$$f(x) = \frac{1}{\sigma \sqrt{2\pi}} e^{-\frac{1}{2}\left(\frac{x - \mu}{\sigma}\right)^2}$$

* **Translation:** *"Scores cluster symmetrically around the average ($\mu$), decaying smoothly as you move further away."*
* **The De-obfuscation:** The fraction at the front is just a constant multiplier so the total area equals $1$. The core mechanism is $e^{-(\text{distance})^2}$.

---

### Matrices & Logic

#### 13. Swapping Two Variables ($a$ and $b$)

$$\begin{bmatrix} a \\ b \end{bmatrix} \leftarrow \begin{bmatrix} 0 & 1 \\ 1 & 0 \end{bmatrix} \begin{bmatrix} a \\ b \end{bmatrix}$$

* **Translation:** `temp = a; a = b; b = temp;`
* **The De-obfuscation:** Multiplying a 2D vector by a anti-diagonal permutation matrix flips the top and bottom values.

#### 14. Checking if $x$ is Even or Odd

$$x \pmod 2 \equiv 0$$

* **Translation:** *"Is $x$ an even number?"*
* **The De-obfuscation:** The remainder when $x$ is divided by $2$ is $0$.

#### 15. The Identity Matrix Operations

$$\det(I_n) = 1^n$$

* **Translation:** $1 = 1$
* **The De-obfuscation:** The determinant of an $n \times n$ identity matrix (a grid with 1s down the diagonal) is simply $1$.
