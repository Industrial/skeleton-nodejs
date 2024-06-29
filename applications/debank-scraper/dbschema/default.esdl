using extension ai;

module default {
  type Wallet {
    required address: str {
      constraint exclusive;
    }
  }

  type NetWorth {
    required wallet: Wallet;
    required date: datetime {
      constraint exclusive;
    }
    required value: float64;
  }
}
