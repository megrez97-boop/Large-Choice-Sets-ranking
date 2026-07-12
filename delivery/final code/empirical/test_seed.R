library(FielDHub)

seed_val <- 12345

cat("=== Generating First Alpha Lattice ===\n")
des1 <- alpha_lattice(t = 16, k = 4, r = 2, seed = seed_val)
block1 <- subset(des1$fieldBook, REP == 1 & IBLOCK == 2)$TREATMENT

cat("=== Generating Second Alpha Lattice ===\n")
des2 <- alpha_lattice(t = 16, k = 4, r = 2, seed = seed_val)
block2 <- subset(des2$fieldBook, REP == 1 & IBLOCK == 2)$TREATMENT

cat("\n[Result Run 1] Rep 1, Block 2 Items: ", paste(block1, collapse=", "), "\n")
cat("[Result Run 2] Rep 1, Block 2 Items: ", paste(block2, collapse=", "), "\n")
cat("\n👉 Are the two designs 100% IDENTICAL? ", identical(block1, block2), "\n")
