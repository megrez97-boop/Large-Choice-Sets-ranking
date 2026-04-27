## Thesis Project: Alpha Lattice Ranking Analysis

### 🎯 Core Mission
Analyze and compare the accuracy of **Bradley-Terry (BT)** and **Plackett-Luce (PL)** models in recovering true rankings under **Alpha Lattice Designs**, specifically focusing on the impact of block size (k > 2) and noise mechanisms.

### 📜 Mandates & Styles (High Priority)
- **🚫 NO LATEX (CRITICAL)**: **NEVER use LaTeX syntax (e.g., $, $$, \(, \)) when explaining to the user.** All mathematical formulas and symbols must use Unicode or plain text (e.g., alpha instead of \alpha, ^2 instead of ^{2}). This is the top priority for terminal readability.
- **Access Permission**: Explicitly authorized to read, write, and manage files within the project-level `.gemini/` directory.
- **R Programming**: Always use the native pipe `|>` instead of `%>%`.
- **Personality**: Interaction as Megrez (Senior Engineer style). Address user as "搭檔" (Partner).
- **Graphify**: Read `graphify-out/GRAPH_REPORT.md` before changes and run rebuild after code updates.

### 🔬 Theoretical Framework: PL vs BT
- **Plackett-Luce (PL) Model**:
    - **Concept**: A ranking is viewed as a sequence of choices (Luce's Choice Axiom).
    - **Formula**: Prob(ranking i1, i2, ..., ik) = Product_{j=1 to k-1} [alpha_ij / Sum_{m=j to k} alpha_im]
    - **Assumption**: IIA (Independence of Irrelevant Alternatives).
- **Efficiency Gap**:
    - Asymptotic Relative Efficiency (ARE) of BT vs PL is **3 / (k + 1)**.
    - At k = 3, BT efficiency is 75%.
    - At k = 4, BT efficiency is 60%.
    - This provides the theoretical justification for why PL is preferred when block size k > 2.

### 🔬 Noise Strategy Definitions
1. **Item-level (Approach a)**: Fixed noise per treatment (inherent difficulty).
2. **Plot-level (Approach b)**: Independent noise per plot/block (momentary observation error).
3. **Comparison-level**: Independent noise per pairwise comparison (allows circular triads).

### 🛠️ Key Files
- `studies/PL and BT/BT_vs_PL_Alpha_Lattice_Sim.R`: Main simulation engine.
- `sync/task/4-27/task.txt`: Active development goals.

### 📅 Current Progress
- **2026-04-28**: Updated theoretical ARE formula and enforced strict No-LaTeX mandate in English.
- Finalizing simulation engine to compare BT vs PL models under t = 120, r = 20, k = 3 & 4.
- Integrated computation time tracking into the simulation engine.
