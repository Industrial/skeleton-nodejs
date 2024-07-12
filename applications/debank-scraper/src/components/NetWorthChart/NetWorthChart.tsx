import { log } from '@code9/log'
import { assertIsNotUndefined, isUndefined } from '@code9/undefined'
import { createAsync } from '@solidjs/router'
import { Chart, Colors, Legend, Title, Tooltip } from 'chart.js'
import { createEffect, createMemo, type JSX } from 'solid-js'

const { WALLET_ADDRESS } = process.env

export const NetWorthChart = (): JSX.Element => {
  assertIsNotUndefined(WALLET_ADDRESS)

  let canvasRef: HTMLCanvasElement | undefined

  const netWorths = createAsync(async () => {
    const response = await fetch(`/api/net-worth?walletAddress=${WALLET_ADDRESS}`)
    const responseJson = await response.json()
    return responseJson
  })

  const netWorthDateSeries = createMemo(() =>
    (netWorths() ?? []).map(({ date }) =>
      date))

  const netWorthValueSeries = createMemo(() =>
    (netWorths() ?? []).map(({ value }) =>
      value))

  const chartData = createMemo(() => {
    return {
      labels: netWorthDateSeries(),
      datasets: [
        // {
        //   label: 'Date',
        //   data: netWorthDateSeries(),
        // },
        {
          label: 'NetWorth',
          data: netWorthValueSeries(),
        },
      ],
    }
  })

  createEffect(() => {
    log.debug({
      method: 'NetWorthChart:createEffect',
    })

    if (isUndefined(canvasRef)) {
      return
    }

    Chart.register(Title, Tooltip, Legend, Colors)

    // eslint-disable-next-line no-new
    new Chart(canvasRef, {
      type: 'line',
      data: chartData(),
      options: {
        responsive: true,
        maintainAspectRatio: false,
        scales: {
          y: {
            beginAtZero: true,
          },
        },
      },
    })
  })

  log.debug({
    method: 'NetWorthChart',
  })

  return (
    <div>
      <canvas ref={canvasRef} width={1024} height={1024}></canvas>
    </div>
  )
}
