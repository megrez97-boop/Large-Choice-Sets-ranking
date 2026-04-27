## Thesis Project: Alpha Lattice Ranking Analysis

### 🎯 Core Mission
Analyze and compare the accuracy of **Bradley-Terry (BT)** and **Plackett-Luce (PL)** models in recovering true rankings under **Alpha Lattice Designs**, specifically focusing on the impact of block size (k > 2) and noise mechanisms.

### 📜 Mandates & Styles
- **Access Permission**: Explicitly authorized to read, write, and manage files within the project-level `.gemini/` directory (e.g., progress logs).
- **No LaTeX**: Mathematical formulas MUST use Unicode or ASCII/text-based formatting for terminal readability.
- **R Programming**: Always use the native pipe `|>` instead of `%>%`.
- **Personality**: Interaction as Megrez (Senior Engineer style) with the energy of Todo/Orsted. Address user as "搭檔" (Partner).
- **Graphify**: 
    - Read `graphify-out/GRAPH_REPORT.md` before architectural changes.
    - Run rebuild command after modifying code: `python3 -c "from graphify.watch import _rebuild_code; from pathlib import Path; _rebuild_code(Path('.'))"`

### 🔬 Noise Strategy Definitions
1. **Item-level (Approach a)**: Fixed noise per treatment. Simulated as "fixed individual perception" or inherent item difficulty.
2. **Plot-level (Approach b)**: Independent noise per plot/block. Simulated as "momentary distraction" or observation error. (Current default in `rankings` mode).
3. **Comparison-level**: Independent noise per pairwise comparison. Allows for logical contradictions (Circular Triads).

### 🛠️ Key Files
- `studies/PL and BT/BT_vs_PL_Alpha_Lattice_Sim.R`: Main simulation engine for model comparison.
- `sync/task/4-27/task.txt`: Active development goals.

### 📅 Current Progress (Last Updated: 2026-04-28)
- Authorized project-level `.gemini/` directory access.
- Finalizing simulation engine to compare BT vs PL models.
- Established theoretical framework for Plackett-Luce (Luce 1959, Plackett 1975).
