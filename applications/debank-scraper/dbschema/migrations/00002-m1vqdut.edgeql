CREATE MIGRATION m1vqdutqhhegtiwf2d6t26yvmnkxivdtolisucrf64hxzw2snt6v2q
    ONTO m1cjba4ed46vfcgf5xich7zjtt7ewellck5v72t7p4t7766pmbf7tq
{
  ALTER TYPE default::Post {
      CREATE REQUIRED PROPERTY body: std::str {
          SET REQUIRED USING ('No content');
      };
  };
};
