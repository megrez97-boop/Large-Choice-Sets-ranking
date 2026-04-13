const fs = require('fs');
const path = require('path');

const skillDir = path.join(__dirname, '..');
console.log(`正在解除安裝 doc-editor-pro (路徑: ${skillDir})...`);

try {
  // 此腳本應由 gemini skills uninstall 呼叫
  console.log('✅ 已確認解除安裝指令。');
  console.log('請手動刪除此資料夾以完成移除，或使用 gemini 指令自動處理。');
} catch (err) {
  console.error(`❌ 解除安裝失敗: ${err.message}`);
}
