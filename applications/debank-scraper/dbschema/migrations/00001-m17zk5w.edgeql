CREATE MIGRATION m17zk5wzb6t6krkutvi62dhcsrt6m3nvbg4ob3kgoaihasjv35ko4q
    ONTO initial
{
  CREATE TYPE default::Wallet {
      CREATE REQUIRED PROPERTY address: std::str;
  };
  CREATE TYPE default::NetWorth {
      CREATE REQUIRED LINK wallet: default::Wallet;
      CREATE REQUIRED PROPERTY date: std::datetime;
      CREATE REQUIRED PROPERTY value: std::float64;
  };
};
