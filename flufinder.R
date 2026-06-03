upload_fasta <- function(fasta_filename) {
  #Opening seqinr library for handling FASTA files; make sure you have seqinr installed
  library(seqinr)
  #Reading the fasta file
  read.fasta(fasta_filename, seqtype = "AA", as.string = TRUE,
             set.attributes = FALSE)
}

splitpeptides_to_masses <- function(aa) {
  # Generating a vector of masses for each amino acid
  aa_masses <- c(A=71.037, R=156.101, N=114.042, D=115.026, C=103.009,
                 Q=128.058, E=129.042, G=57.021, H=137.058, I=113.084, L=113.084,
                 K=128.094, M=131.040, F=147.068, P=97.052, S=87.032, T=101.047,
                 W=186.079, Y=163.063, V=99.068)
  # Looping over each list of peptides and mapping amino acids to masses, returning the sum as peptide masses
  peptide_masses <- aa
  for(i in 1:length(aa)) {
    peptide_masses[[i]] <- lapply(aa[[i]],
                                  function(x) sum(aa_masses[x]))
  }
  # Unlisting the inner lists to generate a list of vectors of masses for each protein
  lapply(peptide_masses, unlist)
}


#function 3
split_peptides <- function(peptides) {
  
  # Opening stringr for simple string manipulation
  #lool
  library(stringr)
  
  #Splitting peptides into individual amino acids using str_split; generates a list of lists of amino acids for each peptide
  lapply(peptides, str_split, pattern="")
  
}

