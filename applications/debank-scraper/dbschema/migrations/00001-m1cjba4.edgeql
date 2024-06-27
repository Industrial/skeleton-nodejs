CREATE MIGRATION m1cjba4ed46vfcgf5xich7zjtt7ewellck5v72t7p4t7766pmbf7tq
    ONTO initial
{
  CREATE TYPE default::Comment {
      CREATE REQUIRED PROPERTY content: std::str;
  };
  CREATE TYPE default::User {
      CREATE REQUIRED PROPERTY name: std::str;
  };
  CREATE TYPE default::Post {
      CREATE REQUIRED LINK author: default::User;
      CREATE REQUIRED PROPERTY title: std::str;
  };
};
