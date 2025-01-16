// @refresh reload
import { StartClient, mount } from '@solidjs/start/client'

const appElement = document.getElementById('app')
if (!appElement) {
  throw new Error('No app element found')
}

mount(() => <StartClient />, appElement)
