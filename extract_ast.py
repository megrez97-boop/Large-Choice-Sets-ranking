import json
from graphify.extract import collect_files, extract
from pathlib import Path

try:
    with open('graphify-out/.graphify_detect.json', 'r', encoding='utf-8') as f:
        detect = json.load(f)
    
    code_files = []
    for f in detect.get('files', {}).get('code', []):
        p = Path(f)
        if p.exists():
            code_files.append(p)

    if code_files:
        result = extract(code_files)
        with open('graphify-out/.graphify_ast.json', 'w', encoding='utf-8') as f:
            json.dump(result, f, indent=2)
        print(f"AST: {len(result.get('nodes', []))} nodes, {len(result.get('edges', []))} edges")
    else:
        empty = {'nodes':[], 'edges':[], 'input_tokens':0, 'output_tokens':0}
        with open('graphify-out/.graphify_ast.json', 'w', encoding='utf-8') as f:
            json.dump(empty, f, indent=2)
        print("No code files - skipping AST extraction")
except Exception as e:
    print(f"Error during AST extraction: {e}")
