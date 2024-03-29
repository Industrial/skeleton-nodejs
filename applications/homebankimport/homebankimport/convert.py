from pandas import DataFrame, Series


def payment(x: str) -> int:
    return {
        "AC": 4,  # AC = Acceptgiro => 4 = Transfer
        "IC": 11,  # IC = Incasso => 11 = Direct Debit
        "BA": 6,  # BA = Betaalautomaat => 6 = Debit card
        "OV": 4,  # OV = Overschrijving => 4 = Transfer
        "PK": 3,  # PK = Opname kantoor => 3 = Cash
        "FL": 4,  # FL = Filiaalboeking => 4 = Transfer
        "PO": 7,  # PO = Periodieke overschrijving => 7 = Standing Order
        "GF": 8,  # GF = Telefonisch bankieren => 8 = Electronic Payment
        "ST": 9,  # ST = Storting => 9 = Deposit
        "GM": 3,  # GM = Geldautomaat => 3 = Cash
        "VZ": 4,  # VZ = Verzamelbetaling => 4 = Transfer
        "GT": 8,  # GT = Internet bankieren => 8 = Electronic Payment
    }.get(
        x, 0
    )  # DV = Diversen => 0 = no payment type assigned


def map_row(row: Series) -> Series:
    date = row["Date"]
    payee = row["Counterparty"]
    debitcredit = row["Debit/credit"]
    amount = float(row["Amount (EUR)"].replace(",", "."))
    if debitcredit == "Credit":
        amount = -abs(amount)
    new_row = Series(
        {
            "date": date,
            "payment": payment(row["Code"]),
            "info": row["Name / Description"],
            "payee": payee,
            "memo": row["Notifications"],
            "amount": amount,
            "category": row["Transaction type"],
            "tags": "",
        }
    )
    return new_row


def convert_dataframe(df_in: DataFrame) -> DataFrame:
    df_out = df_in.apply(map_row, axis=1)
    return df_out
