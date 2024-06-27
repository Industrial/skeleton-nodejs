select User {
  name,
  email
}
filter .email = <str>$email;
