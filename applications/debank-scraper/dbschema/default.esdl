using extension ai;

module default {
  type Wallet {
    required address: str;
  }

  type NetWorth {
    required wallet: Wallet;
    required date: datetime;
    required value: float64;
  }
}
