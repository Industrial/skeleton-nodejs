from homebankimport.convert import convert_dataframe, map_row, payment
from pandas import DataFrame, Series


def test_payment():
    assert payment("AC") == 4
    assert payment("IC") == 11
    assert payment("BA") == 6
    assert payment("OV") == 4
    assert payment("PK") == 3
    assert payment("FL") == 4
    assert payment("PO") == 7
    assert payment("GF") == 8
    assert payment("ST") == 9
    assert payment("GM") == 3
    assert payment("VZ") == 4
    assert payment("GT") == 8
    assert payment("DV") == 0
    assert payment("UNKNOWN") == 0


def test_map_row():
    row = Series(
        {
            "Date": "2022-01-01",
            "Counterparty": "Test",
            "Debit/credit": "Credit",
            "Amount (EUR)": "100,00",
            "Code": "AC",
            "Name / Description": "Test Description",
            "Notifications": "Test Notification",
            "Transaction type": "Test Transaction",
        }
    )
    expected_row = Series(
        {
            "date": "2022-01-01",
            "payment": 4,
            "info": "Test Description",
            "payee": "Test",
            "memo": "Test Notification",
            "amount": -100.00,
            "category": "Test Transaction",
            "tags": "",
        }
    )
    assert map_row(row).equals(expected_row)


def test_convert_dataframe():
    df_in = DataFrame(
        {
            "Date": ["2022-01-01"],
            "Counterparty": ["Test"],
            "Debit/credit": ["Credit"],
            "Amount (EUR)": ["100,00"],
            "Code": ["AC"],
            "Name / Description": ["Test Description"],
            "Notifications": ["Test Notification"],
            "Transaction type": ["Test Transaction"],
        }
    )
    df_out = DataFrame(
        {
            "date": ["2022-01-01"],
            "payment": [4],
            "info": ["Test Description"],
            "payee": ["Test"],
            "memo": ["Test Notification"],
            "amount": [-100.00],
            "category": ["Test Transaction"],
            "tags": [""],
        }
    )
    assert convert_dataframe(df_in).equals(df_out)
