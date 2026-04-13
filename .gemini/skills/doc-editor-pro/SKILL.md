---
name: doc-editor-pro
description: 專業且安全的文件處理技能，支援 PDF、Word (docx) 與 PowerPoint (pptx) 編輯，以及程式碼優化。所有操作皆在本機執行，強制禁止存取敏感檔案如 .env 與 .git。
---

# doc-editor-pro

## 📄 功能概述

本 Skill 提供專業且安全的文件處理能力，專為 PDF、Word (docx) 與 PowerPoint (pptx) 編輯而設計。

## 🛡️ 安全約束 (Mandatory Constraints)

1.  **資料隱私**：所有處理皆在本機進行，禁止調用任何外部 API。
2.  **權限限制**：僅限讀寫您明確指定的 `.pdf`, `.docx`, `.pptx` 檔案。
3.  **禁止行為**：禁止讀取或存取 `.env`, `.git`, `.Rhistory` 或其他隱藏的系統檔案。
4.  **環境限制**：僅在 Python 環境中執行已知安全的庫 (`PyPDF2`, `python-docx`, `python-pptx`)。

## 🛠️ 工作流程

### 1. PDF 編輯
- **讀取/提取**：`pdf_tools.py --action extract --file [FILENAME]`
- **合併**：`pdf_tools.py --action merge --files [FILE1, FILE2]`
- **拆分**：`pdf_tools.py --action split --file [FILENAME] --pages [1-5]`

### 2. Word 編輯 (docx)
- **讀取內容**：`word_tools.py --action read --file [FILENAME]`
- **修改段落**：`word_tools.py --action edit --file [FILENAME] --search [OLD_TEXT] --replace [NEW_TEXT]`
- **新增段落**：`word_tools.py --action add --file [FILENAME] --text [CONTENT]`

### 3. PPT 編輯 (pptx)
- **讀取投影片**：`ppt_tools.py --action read --file [FILENAME]`
- **替換文字**：`ppt_tools.py --action edit --file [FILENAME] --search [OLD_TEXT] --replace [NEW_TEXT]`

### 4. 程式碼編輯與優化
- 使用 Gemini CLI 內建的 `replace`, `grep_search` 工具配合本 Skill 提供的高階重構邏輯：`code_optimizer.py --file [FILENAME] --task [optimize/refactor]`

## ❌ 解除安裝
執行 `gemini skills uninstall doc-editor-pro` 或運行 `scripts/uninstall.cjs` 即可完全移除。
