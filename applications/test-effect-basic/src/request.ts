// request.ts
import { Schema } from 'effect'

// Define a user with an ID and name
export class User extends Schema.Class<User>('User')({
  id: Schema.String, // User's ID as a string
  name: Schema.String, // User's name as a string
}) {}

// Request to retrieve a list of users
export class UserList extends Schema.TaggedRequest<UserList>()('UserList', {
  failure: Schema.Never, // Indicates that no errors are expected
  success: Schema.Array(User), // Specifies that the response is an array of Users
  payload: {},
}) {}

// Request to retrieve a user by ID
export class UserById extends Schema.TaggedRequest<UserById>()('UserById', {
  failure: Schema.String, // Indicates that errors, if any, will be returned as strings
  success: User, // Specifies that the response is a User
  payload: {
    id: Schema.String,
  },
}) {}

// Request to create a new user
export class UserCreate extends Schema.TaggedRequest<UserCreate>()(
  'UserCreate',
  {
    failure: Schema.Never, // Indicates that no errors are expected
    success: User, // Specifies that the response is a User
    payload: {
      name: Schema.String,
    },
  },
) {}
