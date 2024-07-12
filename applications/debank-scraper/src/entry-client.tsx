// @refresh reload
import { log } from '@code9/log'
import { isNull } from '@code9/null'
import { mount, StartClient } from '@solidjs/start/client'

const appElement = document.getElementById('app')
if (isNull(appElement)) {
  throw new Error('No app element found')
}
log.info({
  method: 'entry-client',
})

mount(() =>
  <StartClient />, appElement)
