module default {
  type User {
    required name: str;
  }

  type Post {
    required title: str;
    required body: str;
    required author: User;
  }

  type Comment {
    required content: str;
  }
}
