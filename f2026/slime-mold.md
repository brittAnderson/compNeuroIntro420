---
title: "Does a Slime Mold Think?"
subtitle: "What counts as cognition — and does biology matter?"
author:
  - "Britt Anderson — PSYCH 420, University of Waterloo"
  - "Slides drafted with Claude (Anthropic)"
date: "Computational Modelling for Psychology & Neuroscience"
---


## Where we've been

- **Dog · bell · food** — nobody argues. We grant "it learned."
- **Sunflower · sun · sugar** — anticipation of dawn, but *built in*. Not learned.
- **Pea plant · fan · light** — *claimed* associative learning. Contested; a replication failed.

<!--Notice what moved as we went down the list: not the evidence so much as **how much evidence we demanded**. Our credence tracked the *material*, not the data.

*Ask:* If the same conditioning result showed up in a rat, would you have asked for as much proof?

Now a case that clears the behavioural bar cleanly — and has **no neurons at all**.-->

## A single cell with no brain

*Physarum polycephalum* — a slime mould.

- One giant **multinucleate cell**. No neurons, no synapses, no nervous system.
- Forages by growing a living network of tubes; routes are reinforced or pruned by internal **flow feedback**.
- Long filed under "fungus"; actually an amoeba.

Everything on the next slides is done by *this*.

## It solves mazes

Place it between two food sources in a maze. It fills the maze, then **withdraws from dead ends** and keeps the tube along the **shortest path** connecting the food.

<!-- *Ask:* That's problem-solving. Is it cognition — or just the physics of a flow network settling into a minimum?-->

*— Nakagaki, Yamada & Tóth (2000), Nature*

## It designs networks that rival engineers

Arrange oat flakes as the cities of Greater Tokyo. The mould grows a network that **approximates the real rail system** — balancing total tube length, transport efficiency, and **fault tolerance** against accidental breaks.

Those are the exact tradeoffs human planners spend months negotiating.

<!-- *Ask:* We assume optimization implies a designer with a goal. Does this?-->

*— Tero et al. (2010), Science*

## But is it *learning*?

The sunflower failed this test. The slime mould does not.

- **Habituation** — made to cross a bridge laced with a *harmless* bitter substance, it hesitates at first, then stops reacting over days — and recovers if left alone. Habituation is a textbook elementary form of learning.
- **Anticipation** — shocked with cold at fixed intervals, it slows *before* the next shock — and still slows once at the expected time when the shock is withheld.

<!-- *Ask:* "Learning needs a nervous system." Here it is without one. So is the rule wrong, or was the behaviour never learning? -->

*— Boisseau, Vogel & Dussutour (2016); Saigusa et al. (2008)*

## Memory — outside the body

It avoids ground it has already covered by **sensing the extracellular slime it left behind**. The record of where it has been is written into the *environment*, not stored in a head.

<!-- *Ask:* We locate cognition "in the brain." What do we do with a memory that lives outside the organism? -->

*— Reid, Latty, Dussutour & Beaton (2012), PNAS*

## So: which definition are you using?

No neurons. No brain. One cell. Yet: maze-solving, network optimization, habituation, anticipation, externalized memory.

Two ways to draw the line — and they disagree:

- **Functional** — cognition is what a system *does* (solve, learn, remember). → It qualifies.
- **Substrate** — cognition is a property of certain *stuff / architecture* (nervous systems). → It doesn't.

Same axis as the pea plant, one notch harder — because now the behaviour really is learning. **There is no neutral seat.** Pick one and pay for it.

## The twist for *this* course

Tero's team didn't just watch the mould. They abstracted its behaviour into a **mathematical model** — reinforce tubes carrying more flow, starve the rest — now used to design real-world networks. Others proved formally that *Physarum* **computes shortest paths**.

So the organism *is* an algorithm.

"Does the slime mould think?" collapses into **"is the computation it runs cognition?"** — which is the question this entire course exists to pick apart.

*— Tero et al. (2010); Bonifaci, Mehlhorn & Varma (2012)*

## The whole arc on four axes

"X learns that Y predicts Z" hides four separate claims. Line the cases up against them:

```{=latex}
\scriptsize
```

| Does the case have… | Dog | Sunflower | Pea | Slime mould | LLM |
|---|---|---|---|---|---|
| a real Y→Z contingency | yes | yes | yes | yes | yes |
| behaviour that tracks it | yes | yes | *claimed* | yes | yes |
| acquisition by experience | yes | **no** (built-in) | *claimed* | yes | train: yes / run: no |
| an internal representation | yes? | a clock? | *claimed* | external? | *contested* |

```{=latex}
\normalsize
```

## 

- The **top two rows are free** — a thermostat passes them. "Responds to a predictor" does no work.
- **Acquisition** is where *learning* actually lives: it splits the sunflower (no) from the dog and slime mould (yes), and cuts the LLM in half — trained, then frozen.
- **Representation** — the row people *assume* defines cognition — is the one we can least settle from behaviour. Nearly every cell is a question mark.
- Read the **Pea** and **Dog** columns top to bottom: almost identical. The gap is a failed replication and a hunch about what plants are made of. **That gap is the bias.**

*(A fifth axis — is Z a* reward *(a reinforcer) or merely a benefit? — fractures the same way: food, sugar, light, loss-minimization, an RLHF reward model. Different things, one word.)*

## Your turn — no roles, no notes

# Does a slime mold think?

To argue it, decide what you're actually tracking:

- What evidence would **change your mind** — in either direction?
- Does it matter that it's **alive**?
- If the **identical algorithm** ran in silicon, would your answer change? If yes, your definition is tracking *biology*. If no, it's tracking *computation*. Which did you mean?

## References
```{=latex}
\tiny
```

Nakagaki, T., Yamada, H., & Tóth, Á. (2000). Intelligence: Maze-solving by an amoeboid organism. *Nature*, 407, 470.

Tero, A., Takagi, S., Saigusa, T., Ito, K., Bebber, D. P., Fricker, M. D., Yumiki, K., Kobayashi, R., & Nakagaki, T. (2010). Rules for biologically inspired adaptive network design. *Science*, 327(5964), 439–442.

Saigusa, T., Tero, A., Nakagaki, T., & Kuramoto, Y. (2008). Amoebae anticipate periodic events. *Physical Review Letters*, 100(1), 018101.

Reid, C. R., Latty, T., Dussutour, A., & Beaton, M. (2012). Slime mold uses an externalized spatial "memory" to navigate in complex environments. *Proceedings of the National Academy of Sciences*, 109(43), 17490–17494.

Boisseau, R. P., Vogel, D., & Dussutour, A. (2016). Habituation in non-neural organisms: evidence from slime moulds. *Proceedings of the Royal Society B*, 283(1829), 20160446.

Bonifaci, V., Mehlhorn, K., & Varma, G. (2012). Physarum can compute shortest paths. *Journal of Theoretical Biology*, 309, 121–133.

*Arc context (earlier cases):*

Gagliano, M., Vyazovskiy, V. V., Borbély, A. A., Grimonprez, M., & Depczynski, M. (2016). Learning by association in plants. *Scientific Reports*, 6, 38427.

Markel, K. (2020). Lack of evidence for associative learning in pea plants. *eLife*, 9, e57614.

Atamian, H. S., Creux, N. M., Brown, E. A., Garner, A. G., Blackman, B. K., & Harmer, S. L. (2016). Circadian regulation of sunflower heliotropism, floral orientation, and pollinator visits. *Science*, 353(6299), 587–590.

```{=latex}
\normalsize
```
