# 🧪 Thesis Experimental Studies & Simulation Engines Directory Map

本目錄收錄了本論文研究的**所有實作程式碼、模擬引擎、模型收斂分析與實證評估數據**。整個研究體系分為「隨機效用模擬研究」與「LLM 實證評估平台」兩大核心部分。

---

## 📂 實作研究目錄指引

### 1. 🔬 論文核心模擬引擎與數據生成

#### 🚀 v5.5 最新模擬引擎 (`studies/SLM and alpha lattice expansion/`)
本目錄為控制模擬實驗（Chapter 5）的最新運行核心。
*   `SLM_BT_PL_MultiSeed_Viz.R`：**v5.5 版最新強健模擬引擎**。支援多隨機種子（Multi-Seed）並行模擬、自動繪製 Ribbon 區間圖、內置 Checkpointing 中斷續傳功能，且為 k = 3, 4, 8, 10 區組進行獨立的 Y 軸尺度縮放繪圖。
*   `sim_results_complete_0602_*.rds`：已跑完的完整模擬數據存檔，記錄了 worth 參數恢復精度與擬合時間的 RDS 物件。

#### 💾 遺留模擬腳本與前期模擬 (`studies/PL and BT/` & `studies/alpha lattice + BT/`)
*   `BT_vs_PL_Alpha_Lattice_Sim.R` (位於 `PL and BT/`)：早期版本的 Bradley-Terry 與 Plackett-Luce 在 Alpha Lattice 設計下的模擬比對腳本。
*   `test.R` (位於 `alpha lattice + BT/`)：用於測試 Alpha Lattice 結合 BT 擬合的臨時實驗腳本。

---

### 2. 📊 誤差計算與噪聲分析研究

#### 📈 排序誤差量化與收斂比對 (`studies/error cal/`)
計算模擬worth與真值之偏差（Spearman's Rho / Footrule）。
*   `simulation ranking error.R` & `rho vs footrule, with reverse ranking 4-7.R`：計算不同區組大小 k (2 到 5) 與隨機重複次數 r (20, 30, 50, 100) 下排序指標演進的 R 代碼。
*   `spearman rho, k= 2~5.png` & `spearman footrule, k = 2~5.png`：模擬誤差變化的 ggplot 曲線圖。
*   `graph/`：子目錄，收錄不同 r 與 k 組合下的收斂曲線對照圖。

#### 🎲 Gumbel 噪聲效應研究 (`studies/noise/`)
分析隨機效用理論中，不同噪聲模式（項目級、區組級、成對比較級）的影響。
*   `noise 4-14.R` & `noise 4-14 v2.R`：模擬在高溫（temp = 2.0）與低溫（temp = 0.5）噪聲下，排序恢復精度的變化。
*   `gumbel/`：
    *   `error in rankings or comparisons.R` & `gumbel_plots.R`：在 T = 200, k = 2, 4 條件下，比對「項目級噪聲」與「成對比較級隨機噪聲（產生環形矛盾）」的恢復力。
    *   `gumbel_comparison.pdf`：噪聲理論與模擬結論報告。
    *   `gumbel/plot/`：輸出不同噪聲強度與類型下，Spearman's Rho 及 L1 Footrule 距離的收斂對比圖。

---

### 3. 📐 實驗設計與區組生成測試

#### 🧩 區組設計生成與套件比對 (`studies/alpha lattice 1/`)
*   `alpha design.Rmd` (位於 `agricolae/`)：使用 `agricolae` 套件生成 Alpha 晶格設計的實驗腳本。
*   `Alpha Lattice Simulation.R` & `Simple Alpha Lattice Example.R` (位於 `Feildhub/`)：使用 `FielDHub` 套件生成 Resolvable RIBD (Alpha Lattice Design) 的測試腳本，為我們最終管道採用的設計基礎。

---

### 4. 🧠 LLM 評估引擎與收斂分析平台

#### ⚙️ LLM-as-a-Judge 運行平台核心 (`studies/AI ranking test/` & `studies/LLM_Ranking_Engine_v1/`)
實證示範階段（Chapter 6）的 LLM 評估管道發動機。
*   `scripts/`（及 `LLM_Ranking_Engine_v1/scripts/`）：
    *   `01_API_Gemini_Evaluator.R` (與 `*_r20.R`, `*_t20_r20.R`)：串接 Gemini API 執行 Resolvable Block 評估的主程式，內含選項洗牌與 ID 混淆機制。
    *   `02_Local_Ollama_Evaluator.R`：調用本機 Ollama 運行開源 SLM（Llama 3.2, Phi 4 Mini）的評估程式。
    *   `03_Shiny_App_Evaluator.R`：供人工稽核與單組測試用的互動式 Shiny 應用程式。
    *   `04_Calculate_Global_PL_Ranking.R`：調用 `PlackettLuce` 擬合全局 latent worth 參數並輸出最終 CSV 排名的算法腳本。
*   `data for thesis/`：包含 Practical (N = 152) 與 Creative (N = 160) 數據集分區組配置的原始 Excel 檔案。
*   `results/`：實證輸出結果，包含 Gemini 順利運行的全局 PL 排名檔案（`Global_PL_Ranking_*.csv`、`Success_Gemini_Seed1.csv`）與早期記錄位置偏誤及格式崩潰的 JSON 失敗案例 CSV。
*   `results/raw_jsons/`：儲存 LLM 回傳的原始 block-level 排序 JSON 結果。
*   `archive/fail/`：本機 SLM 評估失敗的檔案紀錄（格式失敗率 15%、Option A Primacy 偏誤 70%、項目遺失等證據）。

#### 📈 重複實驗相似度與收斂研究 (`studies/replication_convergence/`)
評估沒有 Ground-truth 時，模型評估穩定性（Chapter 6.2）的核心。
*   `01_replication_similarity_plot.R`：計算並繪製隨著重複次數 r 增加，重複試驗間的 Kendall's Tau 與 Tie-Aware RBO 的收斂曲線圖。
*   `Replication_Convergence_Plot.png` (與 `*_t20.png`)：頂部重權指標（RBO）與傳統對等指標（Kendall's Tau）的收斂比對折線圖，實證了 Tau 對頂部不穩定性（Apex Swaps）存在評估盲點。
*   `problem/`：子目錄，記錄早期收斂起伏的除錯截圖。
