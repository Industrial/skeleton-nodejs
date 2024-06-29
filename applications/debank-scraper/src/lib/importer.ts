import nodeSchedule from 'node-schedule'

const spec: nodeSchedule.Spec = {
  // hour: 0,
  // minute: 0,
  second: 0,
}

// const spec: nodeSchedule.Spec = '* * * * * *'

const work: nodeSchedule.JobCallback = () => {
  console.log(new Date(), 'This job runs every minute!')
}

let job: nodeSchedule.Job
export const schedule = () => {
  if (job) {
    return
  }

  console.log('Cron spec:', spec)
  console.log('Scheduling job...')

  job = nodeSchedule.scheduleJob(spec, work)
}

schedule()
