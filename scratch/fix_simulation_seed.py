import os

shiny_path = r"C:\Users\user\Documents\R\Thesis\Large-Choice-Sets-ranking\delivery\final code\simulation\BT_vs_PL_Alpha_Lattice_Sim.R"
script_path = r"C:\Users\user\Documents\R\Thesis\Large-Choice-Sets-ranking\delivery\final code\simulation\rho_vs_footrule_no_reverse.R"

# =====================================================================
# 1. Update Shiny App: BT_vs_PL_Alpha_Lattice_Sim.R
# =====================================================================
with open(shiny_path, "r", encoding="utf-8") as f:
    shiny_content = f.read()

# Fix 1: Move set.seed to the very beginning of sim_worker function
old_sim_worker_part = """  # 1. Generate Design
  design <- try(alpha_lattice(t = t_val_local, k = task$k, r = task$r, seed = stable_seed), silent = TRUE)
  if (inherits(design, "try-error")) return(NULL) 
  
  fb <- design$fieldBook
  fb$TrueValue <- as.numeric(fb$ENTRY) 
  
  block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
  blocks <- split(fb, block_tags)
  
  pl_rankings_matrix <- matrix(NA, nrow = length(blocks), ncol = task$k)
  bt_matches_list <- vector("list", length(blocks))
  
  # 重設亂數序列，確保後續噪音與模型擬合的起始狀態與策略無關
  set.seed(stable_seed)"""

new_sim_worker_part = """  # 【重現性關鍵修復】在進行任何隨機操作（包含設計生成與噪音生成）之前，立即設定全局種子以保證重現性
  set.seed(stable_seed)
  
  # 1. Generate Design
  design <- try(alpha_lattice(t = t_val_local, k = task$k, r = task$r, seed = stable_seed), silent = TRUE)
  if (inherits(design, "try-error")) return(NULL) 
  
  fb <- design$fieldBook
  fb$TrueValue <- as.numeric(fb$ENTRY) 
  
  block_tags <- paste(fb$REP, fb$IBLOCK, sep = "_")
  blocks <- split(fb, block_tags)
  
  pl_rankings_matrix <- matrix(NA, nrow = length(blocks), ncol = task$k)
  bt_matches_list <- vector("list", length(blocks))"""

shiny_content = shiny_content.replace(old_sim_worker_part, new_sim_worker_part)

# Fix 2: Add Base Seed input in Shiny UI
old_ui_panel = """      numericInput("r_limit", "Max Replications (r):", 20),
      numericInput("num_seeds", "Number of Seeds:", 5),"""
new_ui_panel = """      numericInput("r_limit", "Max Replications (r):", 20),
      numericInput("base_seed", "Base Seed (起始隨機種子):", 1006),
      numericInput("num_seeds", "Number of Seeds (重複次數):", 5),"""

shiny_content = shiny_content.replace(old_ui_panel, new_ui_panel)

# Fix 3: Update seeds definition in Shiny Server
old_seeds_server = """    run_multi_k_batch(
      t_val = input$t_val, k_values = k_vec, r_limit = input$r_limit, 
      seeds = 1:input$num_seeds, temp_values = temps, """
new_seeds_server = """    run_multi_k_batch(
      t_val = input$t_val, k_values = k_vec, r_limit = input$r_limit, 
      seeds = input$base_seed + (0:(input$num_seeds - 1)), temp_values = temps, """

shiny_content = shiny_content.replace(old_seeds_server, new_seeds_server)

with open(shiny_path, "w", encoding="utf-8") as f:
    f.write(shiny_content)

# =====================================================================
# 2. Update Script: rho_vs_footrule_no_reverse.R
# =====================================================================
with open(script_path, "r", encoding="utf-8") as f:
    script_content = f.read()

# Fix 1: Set worker seed before design generation to ensure replicability
old_script_loop = """    current_k <- task_grid$k[i]
    current_r <- task_grid$r[i]
    
    design <- try(alpha_lattice(t = t_val, k = current_k, r = current_r, seed = seed_val), silent = TRUE)"""

new_script_loop = """    current_k <- task_grid$k[i]
    current_r <- task_grid$r[i]
    
    # 【重現性關鍵修復】設定與當前任務參數綁定的穩定隨機數種子
    worker_seed <- (seed_val * 10000) + (current_k * 100) + current_r
    set.seed(worker_seed)
    
    design <- try(alpha_lattice(t = t_val, k = current_k, r = current_r, seed = worker_seed), silent = TRUE)"""

script_content = script_content.replace(old_script_loop, new_script_loop)

with open(script_path, "w", encoding="utf-8") as f:
    f.write(script_content)

print("Replicability seeds patch applied successfully.")
