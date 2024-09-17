import type { Server } from 'bun'

export type HTTPServerPort = number

export type HTTPRequestHandler = (pathname: string) => Promise<string>

export type StartHTTPServer = (port: HTTPServerPort, handleRequest: HTTPRequestHandler) => Promise<Server>

export const startHTTPServer: StartHTTPServer = async (port = 3000, handleRequest) => {
  console.log('startHTTPServer', port, handleRequest)
  return Bun.serve({
    fetch: async (req: Request) => {
      console.log('startHTTPServer:fetch', req.url)
      const url = new URL(req.url)
      const response = await handleRequest(url.pathname)
      return new Response(response)
    },
    error: (error: Error) => {
      console.log('startHTTPServer:error', error)
      console.error(error)
      return new Response('Internal Server Error', { status: 500 })
    },
    port,
  })
}
