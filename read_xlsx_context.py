import os
import pandas as pd
import openpyxl

p_path = "data/data partition-practical.xlsx"
c_path = "data/data partition-creative.xlsx"
out_path = "scratch_context.txt"

with open(out_path, "w", encoding="utf-8") as f:
    if os.path.exists(p_path):
        f.write("=== Practical Mission Context ===\n")
        try:
            df = pd.read_excel(p_path, sheet_name=1, header=None) # Sheet 2 is index 1
            f.write(str(df.iloc[0, 0]))
            f.write("\n\n")
        except Exception as e:
            f.write(f"Error reading practical: {e}\n\n")
    else:
        f.write("Practical path does not exist\n\n")

    if os.path.exists(c_path):
        f.write("=== Creative Mission Context ===\n")
        try:
            df = pd.read_excel(c_path, sheet_name=1, header=None)
            f.write(str(df.iloc[0, 0]))
            f.write("\n\n")
        except Exception as e:
            f.write(f"Error reading creative: {e}\n\n")
    else:
        f.write("Creative path does not exist\n\n")

print("Done writing context to", out_path)
