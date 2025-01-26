// @refresh reload
import { StartClient, mount } from '@solidjs/start/client'

const appElementId = 'app'
const appElement = document.getElementById(appElementId)

if (!appElement) {
  throw new Error(`No element with id "${appElementId}" found.`)
}

mount(() => <StartClient />, appElement)
