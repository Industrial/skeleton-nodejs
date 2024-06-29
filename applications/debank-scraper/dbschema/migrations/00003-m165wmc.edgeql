CREATE MIGRATION m165wmceceiwhnzmiq25dhwjnk23blih3oovq25wttxuzkvnqmrpza
    ONTO m1zytacrk2uz5sle245zrdym45p7vx2scufsriplmlt4dox5sm4ibq
{
  ALTER TYPE default::NetWorth {
      ALTER PROPERTY date {
          CREATE CONSTRAINT std::exclusive;
      };
  };
  ALTER TYPE default::Wallet {
      ALTER PROPERTY address {
          CREATE CONSTRAINT std::exclusive;
      };
  };
};
