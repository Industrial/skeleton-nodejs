insert NetWorth {
  wallet := (select Wallet filter .address = <str>$walletAddress limit 1),
  date := <datetime>$date,
  value := <float64>$value
};
