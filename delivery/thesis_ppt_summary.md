# 論文 × PPT 對齊工作摘要
> 本文件供下一個對話的 Claude 使用，請直接從「第一章對齊開始」繼續。

---

## 基本資訊

- **論文檔**：Thesis_draft.docx（約 80KB，7章 + References + Appendices）
- **PPT 檔**：GPS_Research_compressed.pdf（343頁，週報形式，2026-01-05 至 2026-07-07）
- **PPT 性質**：每週老師報告用的 Google Slides，以日期分節，記錄研究進程；論文要「以 PPT 為主」對齊。
- **對齊策略**：整份 PPT 都要對到，從第 1 章開始依序進行。

---

## 論文章節結構

| 章 | 標題 | 主要內容 |
|----|------|---------|
| 1 | Introduction | Background, Problem Statement (3個問題), Research Objectives, Contributions |
| 2 | Literature Review | LLM-as-Judge, IBD, PL/BT 模型, RBO 指標 |
| 3 | Empirical Studies（設計章） | 研究流程概述、模擬設計、實證示範設計 |
| 4 | Empirical Study Setup（實作章） | 資料集、模型選擇、系統實作細節 |
| 5 | Simulation Results | BT vs PL 回復力、效率比較、收斂分析 |
| 6 | Observational Results | 位置偏誤、Tau vs RBO 現象、質性驗證 |
| 7 | Discussion | 指標限制、trade-offs、限制、發現總結、實務意義、未來方向 |

---

## PPT 內容按日期摘要

### 2026-01-05（最早）
- 閱讀核心教科書：Oehlert (2010) *A First Course in Design and Analysis of Experiments*
- 介紹 PBIBD 概念（g=8, k=4 範例）

### 2026-02-23
- RBIBD vs BIBD 概念，Round Robin Tournament 類比
- PBIBD 分類：Singular、Semi-Regular、Regular、Triangular、Latin Square、Cyclic
- 結論：Regular PBIBD 對我們最合適
- Alpha Design vs RIBD 比較 → 選 Alpha Design（N=200 時 RIBD 不可行）

### 2026-03-10
- **大修訂（Revision）**：確立研究挑戰：200筆資料 → Alpha Design → BWS/pairwise → BT → 排名
- BT 模型複習：pi/(pi+pj)、MLE、connected graph
- Alpha Design 生成陣列詳細步驟（cyclic substitution, 加 m 倍數）

### 2026-03-17
- Alpha Design 參數 'l'（location）的澄清
- 連結性（connectedness）定義
- FielDHub 套件 alpha lattice 實例

### 2026-03-24
- 'l' vs 'r' 區別：r = 同一組內重複；l = 不同環境/批次
- 模擬誤差結果（k=2 vs k=4，Footrule）
- 程式碼連結

### 2026-03-31
- Spearman's ρ 公式與範例
- Spearman's Footrule
- 應用：比較用戶分組、Split-Half Reliability、Sensitivity Analysis

### 2026-04-07
- 確立假設：Final rank = True rank + error
- Gumbel 分佈：為何用 Gumbel？（extreme value, 對應 logit model）
- 溫度（Temperature）與隨機雜訊的概念
- Footrule vs ρ 比較圖

### 2026-04-14
- 程式碼展示：加誤差於「ranking」vs 加誤差於「comparisons」兩種方法
- 對應程式碼（R）

### 2026-04-21
- 深入 Gumbel 分佈：gumbel(0,1) → gumbel(location, scale)
- 發現：temp * gumbel(0,1) = gumbel(0,temp)（數學上等價）
- Gumbel-Max Trick 與 AI Temperature 的關係
- 討論：**用 AI 可以同時 rank >2 個 treatment → 可改用 PL 模型**

### 2026-04-28
- **引入 Plackett-Luce（PL）模型**
- BT vs PL 公式對比
- PL 的 IIA（Luce's Choice Axiom）假設
- Path Independence 假設

### 2026-05-05
- PL vs BT 模擬結果（seed=1000, temp=10, r=40, t=120）
- k=3, k=4 比較圖
- AI 測試失敗（仍在嘗試 prompt）
- BWS 核心書籍介紹
- BWS 資料如何轉換成排名

### 2026-05-12
- BT vs PL 詳細比較圖（多個條件）
- Efficiency comparison：Zhang (2021) 效率損失公式
- 結論：k > 2 時 PL 優於 BT，效率損失隨 k 增大

### 2026-05-19
- BT 模型實作細節（BradleyTerry2 package, combn(), BTm()）
- PL 模型實作細節（PlackettLuce package, as.rankings(), PlackettLuce()）
- PL 優勢：不切分資料，保留 transitive 資訊

### 2026-05-26
- **PL 模型如何處理 Ties（平手）**（5個步驟）
  - Step 1: Ranking Definition（R = 贏的 cluster 序列）
  - Step 2: Dynamic Pruning（排除從未出現的 tie order）
  - Step 3: Worth Function（tie cluster 用 δ2·(αi·αj)^0.5）
  - Step 4: Iterative MLE
  - Step 5: 解讀 α 參數
- BWS vs PL 對比（傳統 BWS 套件的缺點）

### 2026-06-02
- 模擬結果展示（t=12, 48, 80, 120；k=3,4；r=5,10,15,20）
- AI ranking 程式碼連結（GitHub + Drive）

### 2026-06-16
- 本地模型（Llama 3.2、Phi-4-mini）結果：2 seeds
- Gemini 結果：JSON、PL 結果、程式碼連結
- **RBO (r vs r-1) 圖表**（第一次出現 RBO 結果）

### 2026-06-23
- **Kendall's Rank Correlation** 詳細介紹
  - Measure of Relative Agreement
  - Treatment of Ties as Uncertainty
  - Adjustment Mechanism（Denominator 動態調整）
  - wdm Package
- **Rank-Biased Overlap (RBO)**
  - Top-Heavy Weighting（geometric decay, parameter p）
  - Incomplete & Non-conjoint Lists 處理
  - Ties 的處理方式
- t=12、t=20 的模板圖

### 2026-07-07（最新）
- Creative (t=160, k=4) 和 Practical (t=152, k=4) 完整結果
- **r=20 的 Metric Divergence 案例**（Tau vs RBO 關鍵發現）
  - Tau 穩定在 0.98
  - RBO 跌至 0.77
  - 根本原因：Item 49 worth 從 0.204 → 0.111，Rank 1 → Rank 4
  - Tau 盲點：#1 vs #4 swap 只影響 3/12,720 pairs（0.02%）
- AI 位置偏誤檢查（Creative 和 Practical 各一張）
- 論文大綱連結（Google Doc）

---

## 已知的論文 vs PPT 潛在不一致點（待確認）

以下是在閱讀過程中發現的潛在問題，對齊時重點核查：

### 術語/標籤不一致
1. **章節 2 標題**：論文寫 `# 2. Literature Review [replace with terms]`，這個 placeholder 還沒改。
2. **章節標題缺失**：目錄顯示有 `6.4 Task Dependency: Practical vs. Creative Scenarios` 和 `7.5 Practical Implications`、`7.6 Directions for Future Research`，但對應章節正文要確認是否完整。

### 數字/參數
- PPT 7/7 提到 `3/12,720 discordant pairs`；論文 §7.1 寫 `3 specific pairs` 及 `0.026%` → 12,720 × 0.00026% ≈ 3.3，基本吻合，但論文未直接說 12,720，只說 11,476 pairs（N*(N-1)/2 = 152*151/2 = 11,476）→ **PPT 說 12,720，論文說 11,476，不一致，需確認**。
- PPT 說 Chi-Square df = 9；論文 §6.1.2 也說 df = 9 → 一致（4 positions × 4 ranks - ... 需核算）。

### 模型選擇演進
- PPT 早期（01-05 ~ 04-14）還在用 BT 為主模型，04-21 才引入 PL；論文已全面切換到 PL 為主，BT 為 baseline。PPT 的演進軌跡需要在論文中是否有交代清楚（§2.3.4 有，但可強化）。

### LLM 選擇
- 論文 §4.2 寫 `gemini-3.1-flash-lite`（注意：目前 Gemini 正式名稱是 Gemini 1.5 Flash Lite 或 Gemini 2.0 Flash Lite，需確認版本名稱是否正確）。

---

## 對齊進度記錄

- [x] 閱讀論文全文
- [x] 閱讀 PPT 全文（343頁）
- [x] 產出摘要索引
- [ ] 第 1 章對齊 ← **下次從這裡開始**
- [ ] 第 2 章對齊
- [ ] 第 3 章對齊
- [ ] 第 4 章對齊
- [ ] 第 5 章對齊
- [ ] 第 6 章對齊
- [ ] 第 7 章對齊

---

## 下個對話的銜接 Prompt

```
你好，我在上一個對話已經和你討論過我的論文修改計劃，現在開啟新對話繼續。

【背景】
我在寫關於「用 LLM-as-Judge 配合 Alpha Lattice Design 和 Plackett-Luce 模型做大規模排名」的論文，你幫我對齊論文和老師報告用的 PPT。

【附件】
我會上傳：
1. Thesis_draft.docx（論文本體）
2. GPS_Research_compressed.pdf（老師報告的 PPT，343頁，以週為單位）
3. thesis_ppt_summary.md（上次對話產生的摘要索引，包含兩份文件的重點整理）

【任務說明】
- 論文以 PPT 為主進行對齊
- 如果論文內容和 PPT 有不一致，跟我說並討論怎麼改
- 目前進度：第 1 章對齊尚未開始

【token 管理約定】
- 遇到大量讀取或重複性整理工作，先告訴我，我們決定是否換 Sonnet/Opus 處理後再換回 Fable 5
- 對話變長後如需開新對話，提醒我

【立刻開始】
請讀取三份附件後，從論文第 1 章（Introduction）開始對齊，對照 PPT 中相關內容，列出有哪些不一致或需要改進的地方。
```
