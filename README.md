# deweydatayr
**_R_** API for Dewey Data Inc. Currently supports file download and utility functions.

Explore data at https://www.deweydata.io/.

# API tutorial
### 1. Create API Key
In the system, click Connections → Add Connection to create your API key.

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/8ba869f0-3356-439c-8288-8b8d4bb76326" width="800">
<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/f479586b-ccf4-490b-852c-90051b1e7008" width="600">

As the message says, please make a copy of your API key and store it somewhere. Also, please hit the **Save** button before use.

### 2. Get a product path
Choose your product and Get / Subscribe → Connect to API then you can get API endpoint (product path). Make a copy of it.

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/adc0c8c6-4067-4301-9989-56b21e81f12c" width="800">

### 3. R API
You can install this library directly from the GitHub source as following. It requires `devtools`.
```r
# Load "devtools" library (install it first, if don't have it)
library(devtools)
# Install deweydatar package from GitHub
install_github("Dewey-Data/deweydatar")

# Use deweydatar library
library(deweydatar)

# Start using functions...
# Example, not run
apikey_ = "Your API Key"
pp_ = "Your product path (API endpoint)"
files_df = get_file_list(apikey_, pp_, print_info = T)
download_files(files_df, "C:/Temp")
```

`deweydatar` package has the following functions:

* `get_file_list`: gets the list of files in a `data.frame`
* `read_sample_data`: read a sample of data for a file download URL
* `read_sample_data0`: read a sample of data for the first file with apikey and product path
* `read_local_data`: read data from locally saved csv.gz file
* `download_files`: download files from the file list to a destination folder
* `download_files0`: download files with apikey and product path to a destination folder
* `slice_files_df`: slice a files_df (retrieved by `get_file_list`) by date range

### 4. Examples
I am going to use Advan weekly pattern as an example.
```R
# API Key
apikey_ = "Paste your API key from step 1 here."

# Advan product path
product_path_= "Paste product path from step 2 here."
```
You will only have one API Key while having different product paths for each product.

You can now see the list of files to download by
```r
files_df = get_file_list(apikey_, product_path_, print_info = TRUE)
files_df
```

`print_info = TRUE` set to print the meta information of the files like below:

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/b62c5f2a-bf4b-47f5-a77b-6a7683c09f42" width="600">

Advan weekly pattern data has a total of 8848 files over 9 pages, a total of 1.8TB, and 206.81MB average file sizes.

API v3 introduced the “page” concept that files are delivered on multiple pages. Each page includes about 1,000 files. So, if the data has 8848 files, then there will be 8 pages with 1,000 files each and the 9th page with 848 files. Thus, if you want to download files on pages 2 and 3, you can
```R
files_df = get_file_list(apikey_, product_path_,
                         start_page = 2, end_page = 3, print_info = T)
```
Also, you can do this to download files from page 8 to all the rest
```R
files_df = get_file_list(apikey_, product_path_,
                         start_page = 8, print_info = T)
```
files_df includes a file list (`data.frame`) like below:

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/51315da3-ba77-4988-846a-53b28fd52da3" width="800">
<!--![image](https://github.com/Dewey-Data/deweydatar/assets/142400584/51315da3-ba77-4988-846a-53b28fd52da3) -->
<br><br>
The data.frame has

* `index`: file index ranges from 1 to the number of files
* `page`: page of the file
* `link`: file download link
* `partition_key`: to subselect files based on dates
* `file_name`
* `file_extension`
* `file_size_bytes`
* `modified_at`
* `download_link`: sameas the `link` (`download_link` is left there to be consistent with the v2 tutorial).

You can quickly load/see a sample data by
```R
sample_data = read_sample_data(files_df$link[1], nrows = 100)
```
This will load sample data for the first file in `files_df (files_df$link[1])` for the first 100 rows. You can see any files in the list.

If you want to see the first n rows of the first file skipping `get_file_list`, you can use
```R
sample_data = read_sample_data0(apikey_, product_path_, nrows = 100);
```
This will load the first 100 rows for the first file of Advan data.

Now it’s time to download data to your local drive. First, you can download all the files by

```R
download_files0(apikey_, product_path_, "E:/temp", "advan_wp_")
```
The third parameter is for your destination folder (`E:/temp`), and the last parameter (`advan_wp_`) is the filename prefix. So, all the files will be saved as `advan_wp_xxxxxxx.csv.gz`, etc. You can leave this empty or NULL not to have a prefix.

The second approach to download files is to pass `files_df`:
```R
download_files(files_df, "E:/temp", "advan_wp_")
```
This will show the progress of your file download like below:

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/ddf9c8e4-d6ca-4be4-aa8c-838e740fedf8" width="600">
<!--![image](https://github.com/Dewey-Data/deweydatar/assets/142400584/ddf9c8e4-d6ca-4be4-aa8c-838e740fedf8)-->

If some of the files are already downloaded and if you want to skip downloading them, set 
```R
download_files(files_df, "E:/temp", "advan_wp_", skip_exists = TRUE)
```

Sometimes, the download may stop/fail for any reason in the middle. If you want to resume from the last failure, then you can pass a slice of `files_df`. The progress shows file `index = 2608` for example. If the process was failed on that file, you can resume from that file by
```R
download_files(files_df[files_df$index >= 2608, ], "E:/temp", "advan_wp_")
```
Also, you may want to download incremental files, not all the files from the beginning. Then you can slice the data by date range. For example, to get the file list that falls between `2023-09-01` to `2023-09-10`
```R
sliced_files_df = slice_files_df(files_df, "2023-09-01", "2023-09-10")
```
and to get files from `2023-09-01` to all onward files
```R
sliced_files_df = slice_files_df(files_df, "2023-09-01")
```
and then run
```R
download_files(sliced_files_df, "E:/temp2")
```
You can quickly open a downloaded local file by
```R
sample_local = read_local_data("E:/temp2/advan_wp_Weekly_Patterns_Foot_Traffic-0-DATE_RANGE_START-2019-01-07.csv.gz",
                nrows = 100)
```
Files are large to read from local disk and **_R_**’s base read.csv can be slow. I recommend using `fread` function in the `data.table package`.

Thanks
