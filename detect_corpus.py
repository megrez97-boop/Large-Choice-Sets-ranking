import json
from graphify.detect import detect
from pathlib import Path

try:
    result = detect(Path('.'))
    
    # 手動搜尋所有 .R 檔案並加入 code 類別
    r_files = [str(p) for p in Path('.').rglob('*.R') if '.git' not in p.parts and 'graphify-out' not in p.parts]
    if 'code' not in result['files']:
        result['files']['code'] = []
    
    # 避免重複加入
    existing_code = set(result['files']['code'])
    for rf in r_files:
        if rf not in existing_code:
            result['files']['code'].append(rf)
            result['total_files'] += 1
    
    with open('graphify-out/.graphify_detect.json', 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=2)
    
    print(f"Corpus: {result.get('total_files')} files | ~{result.get('total_words')} words")
    files_info = result.get('files', {})
    for k, v in files_info.items():
        if v:
            print(f"  {k}: {len(v)} files")
except Exception as e:
    print(f"Error during detection: {e}")
