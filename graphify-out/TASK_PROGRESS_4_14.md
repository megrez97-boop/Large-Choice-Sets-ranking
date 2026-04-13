# 任務進度紀錄 (2026-04-14) - 論文模擬優化

## 🎯 核心任務
1. **[✅] 加入 Gumbel 雜訊 (Noise)**
   - 狀態：已完成 (實作於 `noise 4-14.R`)
   - 實作細節：$U_i = V_i + Temp \times rgumbel(n, 0, 1)$
2. **[⏳] 實作 BWS (Best-Worst Scaling) 選項**
   - 狀態：準備開始
   - 目標：讓函數能切換「全區組排名 (Ranking)」或「最佳-最差選擇 (BWS)」。
   - 目標：讓函數能切換「全區組排名 (Ranking)」或「最佳-最差選擇 (BWS)」。

## 🛠️ 實作細節記錄
- **基礎程式碼**: `noise 4-14.R`
- **環境**: R (FielDHub, BradleyTerry2, evd)
- **技術決策**: 使用 `evd::rgumbel` 產生雜訊以節省開發時間。

---
## 📝 學習筆記 (教學記錄)
### 1. 雜訊 (Noise) 的概念
- **實作方式**: 使用 `evd` 套件的 `rgumbel()`。
- **公式**: $U_i = V_i + Temp \times rgumbel(n, 0, 1)$。
- **Temp 的意義**: 當 Temp = 0，代表完美判斷；Temp 愈大，判斷愈混亂。

### 2. BWS 的邏輯
- 全排名 vs. 最佳/最差。
- 對資料量的影響。
