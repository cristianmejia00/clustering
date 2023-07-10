# clustering

The main repo for clustering academic articles, news, and patents using citation networks and topic model.

## Processing:

- Step 1: Create a `data.rdata` file.
  - This file contains the `dataset`. And optionally, the `orphans` and the `network` objects. These are created with any of the codes in the `/01_data_loading` folder.
  - The `data.rdata` file is saved in its query code folder in `C:documents/03_bibliometrics/inputs`
- Step 2: Copy the `settings.r` file and fill it out with the corresponding info.
  - This file is usually the only one we need to save in the research project folder as a backup for the parameters.
- Step 3: Run the file processing code `00_run_me.r`

After all, input data and output reports are saved in the `C:documents/03_bibliometrics` folder.
