# 20221107

# Transform the RTF files obtained from FACTIVA with the code in R-codesFACTIVA
# Run 00, 01, and 02.
# This will create a `data_environ.rdata` file
# Load it here.

# Get the dataset object
dataset <- newsVOS

# Save it
save(dataset, file="data.rdata")

# Copy `data.rdata` file and paste it in its query folder in the bibliometrics folder.
# Usually, you'll need to create the query folder because is the first time we need it. 