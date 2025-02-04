#import data into Rstudio
WholeSCutmr<-read.csv(file.choose(),header = TRUE)

# Display the structure of the dataset
View(WholeSCutmr)
str(WholeSCutmr)

# Total missing values
sum(is.na(WholeSCutmr))  
# Missing values per column
colSums(is.na(WholeSCutmr))  

# Check for missing values and replace them with the mean of the respective column
WholeSCutmr[is.na(WholeSCutmr)] <- sapply(WholeSCutmr, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))

library(dplyr)

# Replace NA values with the column mean only for numeric columns
WholeSCutmr <- WholeSCutmr %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# 1. **Merging Data**
# Assuming an additional dataset "customer_info.csv" containing customer information,
# we merge it with the main dataset based on CustomerID.
customer_info <- data.frame(CustomerID = 1:nrow(WholeSCutmr), 
                            Region = sample(c("North", "South", "East", "West"), nrow(WholeSCutmr), replace = TRUE))

# Add a unique CustomerID column to the main dataset for merging
WholeSCutmr$CustomerID <- 1:nrow(WholeSCutmr)

# Merge the datasets using the common "CustomerID" column
MergedData <- merge(WholeSCutmr, customer_info, by = "CustomerID")

# check columns after merging to avoid errors
colnames(MergedData)  # تأكد من وجود Channel, Region, Fresh, Milk

#وجدت مشكلة في وجود عمود مشترك من الجداول مكرر وهو region
#وحلها كالتالي 

# ✅ 4. Merge data and fix `Region.x` and `Region.y` issue
MergedData <- merge(WholeSCutmr, customer_info, by = "CustomerID", all = TRUE)

# ✅ 5. Fix duplicate columns `Region.x` and `Region.y`
MergedData <- MergedData %>%
  select(-Region.x) %>%
  rename(Region = Region.y)

# ✅ 6. Check columns after merging
colnames(MergedData)

# 2. **Filtering Data**
# Extract customers who purchased more than 10,000 units of fresh products
FilteredData <- subset(MergedData, Fresh > 10000)


# 3. **Subsetting Data**
# Extract specific columns (after verifying their existence)
required_columns <- c("Channel", "Region", "Fresh", "Milk")
existing_columns <- required_columns[required_columns %in% colnames(MergedData)]
SubsetData <- MergedData[, existing_columns, drop = FALSE]

# 4. **Reshaping Data**
# Convert the dataset from wide format to long format using pivot_longer
library(tidyr)
ReshapedData <- pivot_longer(MergedData, 
                             cols = c(Fresh, Milk, Grocery, Frozen,
                                      Detergents_Paper, Delicassen), 
                             names_to = "Product_Type", 
                             values_to = "Amount")

# Display the processed datasets
head(MergedData)    # After merging
head(FilteredData)  # After filtering
head(SubsetData)    # After subsetting
head(ReshapedData)  # After reshaping



# Standardize numeric variables only. قياس المتغيرات الرقمية فقط
numeric_columns <- sapply(data, is.numeric)  #Define numeric columns تحديد الأعمدة الرقمية
data_standardized <- as.data.frame(scale(data[, numeric_columns]))

#View sample benchmark data عرض عينة من البيانات المعيارية
head(data_standardized)


