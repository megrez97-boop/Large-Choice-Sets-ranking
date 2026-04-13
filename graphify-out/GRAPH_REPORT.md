# Graph Report - C:/Users/User/Documents/R/Thesis  (2026-04-14)

## Corpus Check
- 9 files · ~7,683 words
- Verdict: corpus is large enough that graph structure adds value.

## Summary
- 7 nodes · 7 edges · 3 communities detected
- Extraction: 71% EXTRACTED · 29% INFERRED · 0% AMBIGUOUS · INFERRED: 2 edges (avg confidence: 0.9)
- Token cost: 7,000 input · 1,000 output

## God Nodes (most connected - your core abstractions)
1. `GPS Research Workflow` - 3 edges
2. `Bradley-Terry (BT) Model` - 3 edges
3. `Best-Worst Scaling (BWS)` - 2 edges
4. `Random Utility Theory (RUT)` - 2 edges
5. `Gumbel Distribution` - 2 edges
6. `Alpha Lattice Design` - 1 edges
7. `Simulation Ranking Error Chart` - 1 edges

## Surprising Connections (you probably didn't know these)
- `Simulation Ranking Error Chart` --evaluates_accuracy_of--> `Bradley-Terry (BT) Model`  [INFERRED]
  error cal\graph\k=4,r=100.png → GPS Research.pdf

## Communities

### Community 0 - "Core Thesis Concepts"
Cohesion: 0.67
Nodes (3): Bradley-Terry (BT) Model, Simulation Ranking Error Chart, Gumbel Distribution

### Community 1 - "Core Thesis Concepts"
Cohesion: 1.0
Nodes (2): Alpha Lattice Design, GPS Research Workflow

### Community 2 - "Core Thesis Concepts"
Cohesion: 1.0
Nodes (2): Best-Worst Scaling (BWS), Random Utility Theory (RUT)

## Knowledge Gaps
- **2 isolated node(s):** `Alpha Lattice Design`, `Simulation Ranking Error Chart`
  These have ≤1 connection - possible missing edges or undocumented components.
- **Thin community `Core Thesis Concepts`** (2 nodes): `Alpha Lattice Design`, `GPS Research Workflow`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Core Thesis Concepts`** (2 nodes): `Best-Worst Scaling (BWS)`, `Random Utility Theory (RUT)`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **Why does `GPS Research Workflow` connect `Core Thesis Concepts` to `Core Thesis Concepts`, `Core Thesis Concepts`?**
  _High betweenness centrality (0.467) - this node is a cross-community bridge._
- **Why does `Bradley-Terry (BT) Model` connect `Core Thesis Concepts` to `Core Thesis Concepts`?**
  _High betweenness centrality (0.467) - this node is a cross-community bridge._
- **Why does `Best-Worst Scaling (BWS)` connect `Core Thesis Concepts` to `Core Thesis Concepts`?**
  _High betweenness centrality (0.133) - this node is a cross-community bridge._
- **Are the 2 inferred relationships involving `Bradley-Terry (BT) Model` (e.g. with `Gumbel Distribution` and `Simulation Ranking Error Chart`) actually correct?**
  _`Bradley-Terry (BT) Model` has 2 INFERRED edges - model-reasoned connections that need verification._
- **What connects `Alpha Lattice Design`, `Simulation Ranking Error Chart` to the rest of the system?**
  _2 weakly-connected nodes found - possible documentation gaps or missing edges._