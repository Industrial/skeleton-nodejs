#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from 'zx'

await $`sudo docker rm $(sudo docker ps -aq)`
await $`sudo docker rmi $(sudo docker images -aq)`
