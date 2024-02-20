from homebankimport.convert import convert_dataframe
from pandas import DataFrame, read_csv, to_datetime

input_file_path = "/home/tom/Downloads/transactions.csv"
output_file_path = "/home/tom/Downloads/transactions_out.csv"

df_in: DataFrame = read_csv(input_file_path, delimiter=",")
df_in["Date"] = to_datetime(df_in["Date"], format="%Y%m%d")

df_out: DataFrame = convert_dataframe(df_in)
df_out.to_csv(output_file_path, index=False, sep=";")
