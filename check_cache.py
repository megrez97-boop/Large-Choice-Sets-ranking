import json
from graphify.cache import check_semantic_cache
from pathlib import Path

try:
    with open('graphify-out/.graphify_detect.json', 'r', encoding='utf-8') as f:
        detect = json.load(f)
    
    all_files = [f for files in detect['files'].values() for f in files]
    # 移除重複 (例如同一個檔案在 code 和 document 都有)
    all_files = list(set(all_files))

    cached_nodes, cached_edges, cached_hyperedges, uncached = check_semantic_cache(all_files)

    if cached_nodes or cached_edges or cached_hyperedges:
        cache_data = {'nodes': cached_nodes, 'edges': cached_edges, 'hyperedges': cached_hyperedges}
        with open('graphify-out/.graphify_cached.json', 'w', encoding='utf-8') as f:
            json.dump(cache_data, f, indent=2)
            
    with open('graphify-out/.graphify_uncached.txt', 'w', encoding='utf-8') as f:
        f.write('\n'.join(uncached))
        
    print(f"Cache: {len(all_files) - len(uncached)} files hit, {len(uncached)} files need extraction")
except Exception as e:
    print(f"Error during cache check: {e}")
