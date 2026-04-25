# Simple Alpha Lattice Example (t=20, k=4, r=2)
# m = 5 (blocks per replication), g = 20 (total treatments/genotypes)
simple_alpha <- alpha_lattice(
  t = 20, 
  k = 4, 
  r = 2, 
  l = 1, 
  plotNumber = 101, 
  locationNames = "Site1", 
  seed = 42
)

# Show design information
simple_alpha$infoDesign

# Show the first few rows of the field book
head(simple_alpha$fieldBook, 10)