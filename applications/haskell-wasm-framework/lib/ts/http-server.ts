import type { Server } from 'bun'

export type HTTPServerPort = number

export type HTTPRequestHandler = (pathname: string) => Promise<string>

export type StartHTTPServer = (
  port: HTTPServerPort,
  handleRequest: HTTPRequestHandler,
) => Promise<Server>

export const startHTTPServer: StartHTTPServer = async (port, handleRequest) => {
  const usedPort = port ?? 3000

  // eslint-disable-next-line no-console
  console.log('startHTTPServer', usedPort, handleRequest)
  return Bun.serve({
    fetch: async (req: Request) => {
      // eslint-disable-next-line no-console
      console.log('startHTTPServer:fetch', req.url)
      const url = new URL(req.url)
      const response = await handleRequest(url.pathname)
      return new Response(response)
    },
    error: (error: Error) => {
      // eslint-disable-next-line no-console
      console.log('startHTTPServer:error', error)
      // eslint-disable-next-line no-console
      console.error(error)
      return new Response('Internal Server Error', { status: 500 })
    },
    port: usedPort,
  })
}
