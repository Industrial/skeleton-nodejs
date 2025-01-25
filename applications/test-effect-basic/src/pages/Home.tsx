import React from 'react'

export const Home = () => {
  const [users, setUsers] = React.useState([])

  React.useEffect(() => {
    fetch('/rpc', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        type: 'UserList',
        payload: {},
      }),
    })
      .then((response) => response.json())
      .then((data) => setUsers(data))
  }, [])

  return (
    <div>
      <h1>Welcome to the React SSR App!</h1>
      <button onClick={() => console.log('Clicked!')}>Click me!</button>
      <div>
        {users.map((user) => (
          <div key={user.id}>{user.name}</div>
        ))}
      </div>
    </div>
  )
}
