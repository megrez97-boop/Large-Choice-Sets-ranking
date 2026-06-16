library(httr)
library(jsonlite)

message("=========================================")
message("       API KEY 驗證程式啟動")
message("=========================================\n")

# --- 1. 測試 OpenAI API ---
openai_key <- Sys.getenv("OPENAI_API_KEY")
if (openai_key == "") {
  message("❌ [OpenAI] 失敗：在環境變數中找不到 OPENAI_API_KEY。請確認 .Renviron 已經設定並重啟了 R。")
} else {
  message("⏳ [OpenAI] 鑰匙已讀取，正在連線至 OpenAI 伺服器...")
  
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", openai_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o-mini",
      messages = list(list(role = "user", content = "Please reply exactly with: 'OpenAI API is ALIVE!'")),
      max_tokens = 10,
      temperature = 0
    )
  )
  
  if (status_code(res) == 200) {
    parsed <- content(res, as = "parsed", type = "application/json")
    message("✅ [OpenAI] 測試成功！伺服器回應：", parsed$choices[[1]]$message$content)
  } else {
    message("❌ [OpenAI] 連線失敗！HTTP 狀態碼：", status_code(res))
    message("詳細錯誤訊息：")
    print(content(res))
  }
}

message("\n-----------------------------------------\n")

# --- 2. 測試 Gemini API ---
gemini_key <- Sys.getenv("GEMINI_API_KEY")
if (gemini_key == "") {
  message("❌ [Gemini] 失敗：在環境變數中找不到 GEMINI_API_KEY。請確認 .Renviron 已經設定並重啟了 R。")
} else {
  message("⏳ [Gemini] 鑰匙已讀取，正在連線至 Google 伺服器...")
  
  # Gemini 把 API key 放在 URL query 裡面
  gemini_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash:generateContent?key=", gemini_key)
  
  res <- POST(
    url = gemini_url,
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        list(parts = list(list(text = "Please reply exactly with: 'Gemini API is ALIVE!'")))
      ),
      generationConfig = list(temperature = 0)
    )
  )
  
  if (status_code(res) == 200) {
    parsed <- content(res, as = "parsed", type = "application/json")
    message("✅ [Gemini] 測試成功！伺服器回應：", parsed$candidates[[1]]$content$parts[[1]]$text)
  } else {
    message("❌ [Gemini] 連線失敗！HTTP 狀態碼：", status_code(res))
    message("詳細錯誤訊息：")
    print(content(res))
  }
}

message("\n=========================================")
message("       驗證程式結束")
message("=========================================")
