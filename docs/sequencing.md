#Wastewater Genomics Sequencing Analysis For SARS-CoV-2

## Purpose

This documentation page outlines the procedure for processing and analyzing wastewater sequencing data as part of the wastewater genomics surveillance program. The goal is to ensure consistent, accurate, and reliable data for public health monitoring and decision-making.

## Definitions

- **BWA**: BWA is a software package used to map sequences with low divergence to a sizable reference genome, like the Wuhan-hu SARS-CoV-2 genome. Three algorithms make up this system: BWA-backtrack, BWA-SW, and BWA-MEM.
- **Exatype**: Exatype is a software platform developed by Hyrax Biosciences designed to analyze genetic sequences of viruses and other pathogens. Exatype could help in determining how these viruses are mutating and potentially inform the development of treatment strategies and vaccines by identifying mutations that might confer resistance to existing therapies.
- **Freyja**: Freyja is a tool to recover relative lineage abundances from mixed SARS-CoV-2 samples from a sequencing dataset (BAM aligned to the Hu-1 reference). The technique solves the limited (unit sum, non-negative) de-mixing problem by using lineage-determining mutational “barcodes” produced from the UShER global phylogenetic tree as a basis set.
- **Mutational profile**: Refers to the characterization of mutations present within a genome, gene, or set of genes in an organism. This profile includes the types, locations, and frequencies of genetic alterations, providing insights into the molecular underpinnings of various conditions, diseases, or evolutionary processes.
- **NGS**: Next-generation Sequencing, a method used for precise and comprehensive analysis of the genetic material found in wastewater samples.
- **Samtools**: Samtools is a set of tools used to interact with high-throughput sequencing data.
- **iVar**: iVar is a software package that consists of functions broadly useful for viral amplicon-based sequencing.
- **Wastewater sample**: A sample collected from various wastewater catchment sites, intended for genomic analysis.


All commands/scripts must be executed on a Linux server/environment with Conda activated.

##Data Preprocessing
   Raw sequence data preprocessing steps including quality control, trimming of adapters, and removal of low-quality sequences are performed by the NICD Sequencing Core Facility and are therefore not included in this documentation page.

###Alignment
   - **Aligning reads to reference SARS-CoV-2 genome**:

This script performs alignment of raw sequencing reads to the reference SARS-CoV-2 genome using the BWA (Burrows-Wheeler Aligner) tool.
Before running, the input and output directories of all files are first defined. 

This line defines the path to the reference genome sequence file.
The reference genome is the known sequence of the SARS-CoV-2 genome to which the raw sequencing reads will be aligned.

     # Directory where your indexed reference sequence is
	REF=”/path/to/reference/sequence.fa

This line defines the directory where the raw sequencing files (in fastq.gz format) are located.
These are the files containing the raw reads obtained from sequencing the wastewater samples.

	RAW_DIR="/path/to/raw_files"

This line defines the output directory where the aligned SAM files will be saved. 
SAM files (Sequence Alignment/Map) store the alignment information of reads to a reference genome.

	OUT_DIR="./mapped_epi6_8"

This command creates the output directory defined in OUT_DIR if it does not already exist. 
The -p flag ensures that the command does not throw an error if the directory already exists.

	mkdir -p "$OUT_DIR"


A for loop is created to iterate over each R1 fastq.gz file in the raw files directory (RAW_DIR). R1 fastq.gz files contain the forward reads 
obtained from sequencing.The filename of the R1 file is then modified to generate the corresponding R2 filename. 
In Illumina paired-end sequencing, R1 and R2 files contain paired-end reads, where R1 contains the forward reads and R2 contains the reverse reads. 
This assumes a consistent naming convention where R1 files end with "_R1_001.fastq.gz" and R2 files end with "_R2_001.fastq.gz".

	for R1 in "$RAW_DIR"/*_R1_001.fastq.gz; do
    # Assuming the naming convention is consistent, modify the suffix to find the R2 files
    	R2="${R1/_R1_/_R2_}"
    # Extract the sample name based on the R1 file
    	SAMPLE_NAME=$(basename "$R1" "_R1_001.fastq.gz")
    # Construct the output file name
    	SAM_FILE="${OUT_DIR}/${SAMPLE_NAME}.aligned.sam"

This line prints a message indicating which sample is being mapped.

    echo "Mapping $SAMPLE_NAME..."

his line executes the BWA MEM algorithm to align the paired-end reads ($R1 and $R2) to the reference genome ($REF). 
The alignment results are then redirected (>) to the output SAM file ($SAM_FILE).

    bwa mem "$REF" "$R1" "$R2" > "$SAM_FILE"

This marks the end of the loop.

	done

This line prints a message indicating that the mapping process for all samples is complete.

	echo "Mapping complete."

  
###Converting SAM files to BAM file, and sorting the BAM files**:
     ```bash
     # Script available in the documentation
     ```

###Read Trimming, Variant Calling, and Analysis:
   - **Trimming the reads and sorting**:

This script will loop through all indexed BAM files in the indexed directory and then extract the base filename without the .sorted.bam extension. Ultimately it will define the output trimmed BAM file path and output trimmed and sorted BAM file path

     for INDEXED_FILE in "$INDEXED_DIR"/*.sorted.bam; do
     BASENAME=$(basename "$INDEXED_FILE" .sorted.bam)
     TRIMMED_FILE="$TRIMMED_DIR/${BASENAME}.trimmed.bam"
     
This line will now trim the reads for each sample

     echo "Trimming reads for $INDEXED_FILE"
     ivar trim -b nCoV-2019_v1.bed -i "$INDEXED_FILE" -p "$TRIMMED_FILE" -q 15 -m 100 -s 4 -      e -x 3
     
This line will sort the trimmed reads

     echo "Sorting trimmed reads for $TRIMMED_FILE"
     samtools sort "$TRIMMED_FILE" -o "$TRIMMED_SORTED_FILE"
     done
     echo "Read trimming and sorting complete."



###Freyja Analysis:
   - **Running Freyja variants**:
   - 
This script will specify the directory where the variants and depths files are located.

      VARIANTS_DIR="/RAW/Depths_TSV_all_copy_B/TSV_all/TSV"
      DEPTHS_DIR="/RAW/Depths_TSV_all_copy_B/Depths_all/Depths"

This will create the output directory (if not yet created) where the results will be stored

      OUTPUT_DIR="/RAW/Depths_TSV_all_copy_B/output"
      mkdir -p "${OUTPUT_DIR}"

Loop into the variant files in the variants directory and then extract the base name for matching with the depth file. It will then construct the path to the matching depth file and subsequently specify the output file name, ending with .demix.tsv.

      for variants_file in ${VARIANTS_DIR}/*.tsv; do
      base_name=$(basename "${variants_file}" .tsv)
      depth_file="${DEPTHS_DIR}/${base_name}.depths"
      output_file="${OUTPUT_DIR}/${base_name}.demix.tsv"

This script will check if the depth file exists(not compulsory) then run the Freyja variants step.

      if [ ! -f "${depth_file}" ]; then
     echo "Depth file for ${base_name} does not exist. Skipping..."
     continue
     fi
     #Run Freyja demix
     echo "Processing: ${base_name}"
     freyja demix "${variants_file}" "${depth_file}" --output "${output_file}"
     done
     echo "All samples processed."


   - **Freyja Demix**:

This script will iterate over each TSV file created on the previous step then it runs the Freyja demix step using the tsv file and depths file.

       For tsv_file in /RAW/Depths_TSV_all_copy_B/TSV_all/TSV/*.tsv; do
       sample_name=$(basename “tsv_file” .tsv)
       depths_file= “/RAW/Depths_TSV_all_copy_B/Depths/${sample_name}.depths”
       output_file=”RAW/Freyja_demix/${sample_name}.demix.tsv
       echo “Running Freyja for $tsv_file”
       Freyja demix “$tsv_file” “$depths_file” –output “output_file”
       done.

     
   - **Aggregating the Freyja Output Files**:

      freyja aggregate /RAW/Freyja_demix/ --ext .tsv –output                           /RAW/Freyja_demix/aggregate_file/agg_epi2_8.tsv
     


#Mutational and SNPs Analysis:
   - **Exatype Parameters**:
     ```
     Data Structure: Paired-end
     Sequencing Platform: Illumina
     Assay: ARTIC V4
     ```
   - **Generate Heatmap and Mutational Profile**:
     ```R
     # Script available in the documentation
     ```

## References

- Castellano, Sara, et al. “iVar, an Interpretation-Oriented Tool to Manage the Update and Revision of Variant Annotation and Classification.” *Genes* 12, no. 3 (March 8, 2021): 384. [DOI: 10.3390/genes12030384](https://doi.org/10.3390/genes12030384)
- Dahui, Qin. “Next-Generation Sequencing and Its Clinical Application.” *Cancer Biology & Medicine* 16, no. 1 (February 1, 2019): 4–10. [DOI: 10.20892/j.issn.2095-3941.2018.0055](https://doi.org/10.20892/j.issn.2095-3941.2018.0055)
- Grubaugh, N.D., Gangavarapu, K., Quick, J. et al. An amplicon-based sequencing framework for accurately measuring intrahost virus diversity using PrimalSeq and iVar. Genome Biol 20, 8 (2019). https://doi.org/10.1186/s13059-018-1618-7
- Karthikeyan, Smruthi, et al. “Wastewater Sequencing Reveals Early Cryptic SARS-CoV-2 Variant Transmission.” *Nature* 609, no. 7925 (September 1, 2022): 101–8. [DOI: 10.1038/s41586-022-05049-6](https://doi.org/10.1038/s41586-022-05049-6)
- Li, Heng, and Richard Durbin. “Fast and Accurate Short Read Alignment with Burrows–Wheeler Transform.” *Bioinformatics* 25, no. 14 (July 15, 2009): 1754–60. [DOI: 10.1093/bioinformatics/btp324](https://doi.org/10.1093/bioinformatics/btp324)
- Yousif, Mukhlid, et al. “SARS-CoV-2 Genomic Surveillance in Wastewater as a Model for Monitoring Evolution of Endemic Viruses.” *Nature Communications* 14, no. 1 (October 10, 2023): 6325. [DOI: 10.1038/s41467-023-41369-5](https://doi.org/10.1038/s41467-023-41369-5)

