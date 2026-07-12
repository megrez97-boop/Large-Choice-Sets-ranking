# 📚 Thesis Literature Library Directory & Reference Map

本目錄為本論文的研究文獻庫，收錄了**隨機效用理論、偏好選擇模型（Bradley-Terry, Plackett-Luce）、不完全區組設計（BIBD/Alpha Lattice）、排序評估指標（RBO, Kendall's Tau）以及 Best-Worst Scaling (BWS)** 的經典理論教材、學術論文、R 語言官方套件指南與實作代碼。

---

## 📂 核心文獻分類指引

### 1. ⚖️ Bradley-Terry (BT) 偏好模型相關 (`Related paper/BT package/`)
本目錄收錄了成對比較（Pairwise Comparison）的基石理論、估計算法與 R 語言套件實作。
*   `BT祖師爺 Bradley-RankAnalysisIncomplete-1952.pdf` (Bradley & Terry, 1952)：**奠定基礎經典文獻**，提出 Bradley-Terry 模型以處理不完全區組中的成對比較。
*   `1209.1688v4.pdf` (Hunter, 2004)：介紹用於廣義 Bradley-Terry 模型的 **MM 算法** (Minorization-Maximization)，奠定了參數快速收斂的計算基礎。
*   `BT2.pdf` (Turner & Firth, 2012)：R 語言 `BradleyTerry2` 官方套件手冊，詳細說明如何擬合含有協變量的成對比較模型。
*   `github-com-EllaKayeBradleyTerryScalable.pdf` & `btscalable.pdf` / `btscalable.Rmd`：介紹適用於大型稀疏數據的可擴展 BT 模型套件 `BradleyTerryScalable` 的文件與 R 實作腳本。
*   `ELO.docx`：簡述傳統 Elo 評分系統的更新規則與其相較於全局最大概似估計（MLE）的限制。
*   `Choice-Modeling-Methods-and-Pairwise-Comparisons.pdf` / `pairwise comparison.pptx`：關於成對比較方法與選擇模型擬合的教學投影片。
*   `BT.R` & `Bradleyterryscalable.R`：擬合成對比較 worth 參數的實作代碼。

### 2. 📊 Plackett-Luce (PL) 偏好模型相關 (`Related paper/PL package/`)
本目錄收錄了將成對比較擴展至多項排序（Listwise Ranking）的經典模型文獻。
*   `PlackettLuce.pdf` (Turner et al., 2020)：R 語言 `PlackettLuce` 官方套件指南，說明如何藉由 Generalized Bradley-Terry 框架高效擬合含有平手（Ties）或不完整排名的數據。
*   `Plackett_Luce_Model_Comprehensive_Guide.pdf`：Plackett-Luce 模型的綜合性理論與計算指南，包含似然函數推導與演算法收斂機制。

### 3. 🔍 BT vs. PL 理論效率比較 (`Related paper/BT vs PL efficiency/`)
本目錄收錄了對比成對比較（BT）與多項選擇（PL）模型的統計效率及方法論盲點的關鍵文獻。
*   `Building_upon_Bradley-Terry_an.pdf` / `Building upon Bradley-Terry and Plackett-Luce.pdf` (**Zhang, 2021**)：**哈佛大學博士論文**（Sanqian Zhang 著）。這是本論文中排除 Bradley-Terry 模型的理論防禦核心來源。證明了將區組大小 k > 2 的數據強行拆為對子會違背觀測獨立性，進而低估標準誤；並導出了漸近相對效率（ARE）公式 `3 / (k + 1)`，說明效率損失隨區組大小 k 增加而加劇。
*   `BT_vs_PL_Model_Comprehensive_Theoretical_Report.pdf`：評估與對比 BT 與 PL 模型擬合效能的詳細理論研究報告。

### 4. 📐 實驗區組設計相關 (`Related paper/BIBD/`)
本目錄收錄了當候選項目集龐大時，用以分流與包裝數據的實驗設計理論（特別是不完全區組設計 IBD）。
*   `en.wikipedia.org-Block design - Wikipedia.pdf`：實驗設計中區組設計（Block Design）的維基百科理論基礎介紹。
*   `BIBD教學/`：
    *   `ISMS_1953_77 Tables of Two-Associate-Class Partially Balanced Designs.pdf`：PBIBD 經典設計矩陣表格。
    *   `fcdae.pdf` & `fcdae-377-405.pdf`：實驗設計教科書章節，涵蓋不完全區組設計與統計建模。
    *   `alpha design/`：包含 Alpha Lattice Design 的建構原理與區組隨機化調配的教學資料。
*   `Alpha Lattice Simulation.R`：實作不完全區組設計生成與連通性檢驗的 R 程式碼。

### 5. 🧠 LLM 與長文本認知限制 (`Related paper/LLM cons —— long context problem/` & `Related paper/LLm as judge/`)
本目錄收錄了 LLM 做為評估者（LLM-as-a-Judge）時面臨的認知負荷限制文獻。
*   `Lost in the Middle- How Language Models Use Long Contexts.pdf` (Liu et al., 2024)：**核心引文**。證明了 LLM 在長提示詞中部份資訊的注意力會嚴重衰退（"Lost in the Middle" 現象）。該文獻支持了我們限製單一評估區組大小（k = 3 或 4）的決策。
*   `2306.05685v4.pdf` (**Zheng et al., 2023**)：**LLM-as-a-Judge 奠定基礎經典文獻**。MT-Bench 與 Chatbot Arena 的發布論文。系統性證明了 GPT-4 做裁判與人類專家的一致性達 80%+，並首創界定了 LLM 裁判的位置偏誤（Position Bias）、冗長偏誤（Verbosity Bias）與開源小模型做裁判的格式崩潰漏洞，為我們本研究的位置偏誤防禦與小模型失敗分析提供了最直接的文獻根基。

### 6. 🎯 排序相似度與收斂指標 (`Related paper/RBO and kendalls tau/`)
本目錄收錄了如何評估兩個或多個排序結果相似性與穩定性的計量指標。
*   `RBO.pdf` (Webber et al., 2010)：**經典引文**。提出 Rank-Biased Overlap (RBO) 指標，透過幾何衰減賦予頂部高權重（Top-heavy），用以衡量無限列表的相似度。
*   `068-treatment-ties-rank-biased-overlap.pdf` (**Corsi & Urbano, 2024, SIGIR**)：**最新引文**。針對存在「平手（Ties）」或相同 latent worth 分數的排序列表，提出了 Tie-Aware RBO 的優化公式。本研究的 RBO 程式碼即基於此公式開發。
*   `statisticshowto.com-Kendalls Tau...pdf`：Kendall's Tau 排序相關係數的實例計算指引，用以與 RBO 的頂部加權進行對照。
*   `wdm.pdf` (Weighted Distance Metrics)：加權距離指標論文，探討非等權重排序相似性度量。

### 7. 🎲 隨機效用模型與極值分佈 (`Related paper/gumbel/` & `Related paper/wiki/`)
本目錄探討離散選擇模型（Discrete Choice Models）的微觀計量經濟學基礎。
*   `Kenneth Train Discrete Choice Methods with Simulation.pdf` (**Kenneth Train 經典教科書**，2009)：系統性推溫了隨機效用理論（RUM），證明了當隨機干擾項獨立且服從標準 Gumbel（Extreme Value Type I）分佈時，選擇概率會自然轉化為 Multinomial Logit（即 Plackett-Luce）形式。
*   `Kotz, S., & Nadarajah, S. (2000). Extreme Value Distributions...pdf`：極值分佈的機率密度、累積分布與極限性質的數學專著。
*   `en.wikipedia.org-Gumbel distribution - Wikipedia.pdf`：Gumbel 分佈基礎統計特性的快速參考指南。

### 8. 📐 Best-Worst Scaling (BWS) 相關 (`Related paper/BWS/`)
本目錄收錄了最佳-最劣縮放（BWS, Case 1 / BIBD 基底）的調查與評估方法。
*   `Best-worst scaling _ theory, methods and applications...pdf` (Louviere et al., 2015)：**BWS 的權威教科書**。詳細闡述了如何將 IBD 與 BWS 結合以進行大尺度偏好測量。
*   `OSF _ bwsTools Paper.pdf` (位於 BIBD 目錄內)：R 語言中 `bwsTools` 的實作手冊與理論基礎。

### 9. 💼 經典計量經濟學參考 (`Related paper/Manski-DanielMcFaddenEconometric-2001.pdf`)
*   諾貝爾計量經濟學獎得主 Daniel McFadden 與 Charles Manski 聯袂撰寫的離散選擇與經濟行為建模論文。

### 10. 📊 經典排序相關係數歷史文獻 (`Related paper/Spearman-ProofMeasurementAssociation-1904.pdf` & `Rank Correlation Methods...Gibbons...5th ed.pdf`)
*   Spearman 於 1904 年奠定排序相關係數的經典文獻，以及 Maurice Kendall 經典的排序相關係數聖經教材（Gibbons 第五版修訂版）。
